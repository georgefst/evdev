{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -fno-state-hack #-}

-- | The main module for working with devices and events.
module Evdev (
    -- * Devices
    Device,
    newDevice,
    nextEvent,
    evdevDir,
    -- ** Properties
    deviceName,
    devicePath,
    deviceProperties,
    deviceEventTypes,
    deviceHasEvent,
    deviceFd,
    devicePhys,
    deviceUniq,
    deviceProduct,
    deviceVendor,
    deviceBustype,
    deviceVersion,
    deviceAbsAxis,
    AbsInfo (..),
    -- ** Grabbing
    grabDevice,
    ungrabDevice,

    -- * Events
    Event(..),
    EventData(..),
    KeyEvent(..),
    EventCode(..),
    EventValue(..),

    -- * Lower-level
    newDeviceFromFd,
    nextEventMay,
    LEDValue(..),
    setDeviceLED,
    -- ** C-style types
    -- | These correspond more directly to C's /input_event/ and /timeval/.
    -- They are used internally, but may be useful for advanced users.
    Raw.Input_event(..),
    toCEvent,
    fromCEvent,
    toCEventData,
    fromCEventData,
    Raw.Timeval(..),
    toCTimeVal,
    fromCTimeVal,
) where

import Control.Arrow ((&&&))
import Control.Monad (filterM, join)
import Data.ByteString.Char8 (ByteString, pack)
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Int (Int32)
import Data.List.Extra (enumerate)
import Data.Map ((!?), Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (DiffTime)
import Data.Tuple.Extra (uncurry3)
import Data.Word (Word16)
import Foreign (alloca, (.|.), peek, ForeignPtr, withForeignPtr)
import Foreign.C (CInt (CInt), CUInt (CUInt), CUShort (CUShort), Errno (Errno), eAGAIN, eOK)
import Foreign.C.ConstPtr (ConstPtr (ConstPtr), unConstPtr)
import System.Posix.Process (getProcessID)
import System.Posix.Files (readSymbolicLink)
import System.Posix.ByteString (Fd, RawFilePath)
import System.Posix.IO.ByteString (OpenMode (..), defaultFileFlags, openFd)

import qualified Evdev.LowLevel as LL
import qualified Evdev.Raw as Raw
import Evdev.Codes
import Util

-- stores path that was originally used, as it seems impossible to recover this later
-- We don't allow the user to access the underlying low-level C device.
-- | An input device.
data Device = Device { cDevice :: ForeignPtr Raw.Libevdev, devicePath :: ByteString }


instance Show Device where
    show = show . devicePath

-- | An input event, including the timestamp.
data Event = Event
    { eventData :: EventData
    , eventTime :: DiffTime
    }
    deriving (Eq, Ord, Show, Read)

-- | An input event, without the timestamp.
-- Each constructor corresponds to one [event type](https://www.kernel.org/doc/html/latest/input/event-codes.html#event-types), except for 'UnknownEvent'.
data EventData
    = SyncEvent SyncEvent
    | KeyEvent Key KeyEvent
    | RelativeEvent RelativeAxis EventValue
    | AbsoluteEvent AbsoluteAxis EventValue
    | MiscEvent MiscEvent EventValue
    | SwitchEvent SwitchEvent EventValue
    | LEDEvent LEDEvent EventValue
    | SoundEvent SoundEvent EventValue
    | RepeatEvent RepeatEvent EventValue
    | ForceFeedbackEvent EventCode EventValue
    | PowerEvent EventCode EventValue
    | ForceFeedbackStatusEvent EventCode EventValue
    | UnknownEvent Word16 EventCode EventValue {- ^ We include this primarily so that 'fromCEvent' can be well-defined -
        let us know if you ever actually see one emitted by a device, as it would likely
        indicate a shortcoming in the library. -}
    deriving (Eq, Ord, Show, Read)

-- | A direct representation of the /code/ field of the C /input_event/, for when there is no obvious meaningful sum type.
newtype EventCode = EventCode Word16
    deriving (Eq, Ord, Show, Read, Enum)
-- | A direct representation of the /value/ field of the C /input_event/, for when there is no obvious meaningful sum type.
newtype EventValue = EventValue Int32
    deriving (Eq, Ord, Show, Read, Enum)

-- | The status of a key.
data KeyEvent
    = Released
    | Pressed
    | Repeated
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data ReadFlag = Sync | Normal | ForceSync | Blocking
    deriving (Eq, Ord, Show)

convertFlags :: Set ReadFlag -> CUInt
convertFlags = foldr ((.|.) . (.unwrap) . convert) 0
  where
    convert = \case
        Sync -> Raw.LIBEVDEV_READ_FLAG_SYNC
        Normal -> Raw.LIBEVDEV_READ_FLAG_NORMAL
        ForceSync -> Raw.LIBEVDEV_READ_FLAG_FORCE_SYNC
        Blocking -> Raw.LIBEVDEV_READ_FLAG_BLOCKING

defaultReadFlags :: Set ReadFlag
defaultReadFlags = Set.fromList [Normal, Blocking]

nonBlockingReadFlags :: Set ReadFlag
nonBlockingReadFlags = Set.fromList [Normal]

-- | Prevent other clients (including kernel-internal ones) from receiving events. Often a bad idea.
grabDevice :: Device -> IO ()
grabDevice = grabDevice' Raw.LIBEVDEV_GRAB
-- | Release a grabbed device.
ungrabDevice :: Device -> IO ()
ungrabDevice = grabDevice' Raw.LIBEVDEV_UNGRAB

-- | Get the next event from the device.
nextEvent :: Device -> IO Event
nextEvent dev =
    cErrCall "nextEvent" dev $ withForeignPtr (cDevice dev) \devPtr -> alloca \evPtr ->
    (,)
        <$> (Errno <$> Raw.libevdev_next_event devPtr (convertFlags defaultReadFlags) evPtr)
        <*> (fromCEvent <$> peek evPtr)

{- | Get the next event from the device, if one is available.
Designed for use with devices created from a non-blocking file descriptor. Otherwise equal to @fmap Just . nextEvent@.
-}
nextEventMay :: Device -> IO (Maybe Event)
nextEventMay dev =
    cErrCall "nextEventMay" dev $ withForeignPtr (cDevice dev) \devPtr -> alloca \evPtr -> do
    err <- Raw.libevdev_next_event devPtr (convertFlags nonBlockingReadFlags) evPtr
    if Errno err /= eOK
        then
            pure
                ( if Errno -err == eAGAIN then eOK else Errno err
                , Nothing
                )
        else (eOK,) . Just . fromCEvent <$> peek evPtr

fromCEvent :: Raw.Input_event -> Event
fromCEvent Raw.Input_event{type', code, value, time} =
    Event
        (fromCEventData (coerce type', coerce code, coerce value))
        (fromCTimeVal time)

fromCEventData :: (Word16, Word16, Int32) -> EventData
fromCEventData (t, EventCode -> c, EventValue -> v) = fromMaybe (UnknownEvent t c v) $ toEnum' t >>= \case
    EvSyn -> SyncEvent     <$> toEnum' c
    EvKey -> KeyEvent      <$> toEnum' c <*> toEnum' v
    EvRel -> RelativeEvent <$> toEnum' c <*> pure v
    EvAbs -> AbsoluteEvent <$> toEnum' c <*> pure v
    EvMsc -> MiscEvent     <$> toEnum' c <*> pure v
    EvSw  -> SwitchEvent   <$> toEnum' c <*> pure v
    EvLed -> LEDEvent      <$> toEnum' c <*> pure v
    EvSnd -> SoundEvent    <$> toEnum' c <*> pure v
    EvRep -> RepeatEvent   <$> toEnum' c <*> pure v
    EvFf  -> Just $ ForceFeedbackEvent c v
    EvPwr -> Just $ PowerEvent c v
    EvFfStatus -> Just $ ForceFeedbackStatusEvent c v

toCEvent :: Event -> Raw.Input_event
toCEvent (Event e time) = uncurry3 (Raw.Input_event $ toCTimeVal time) (coerce $ toCEventData e)

toCEventData :: EventData -> (Word16, Word16, Int32)
toCEventData = \case
    -- from kernel docs, 'EV_SYN event values are undefined' - we always seem to see 0, so may as well use that
    SyncEvent                (fromEnum' -> c) -> (fromEnum' EvSyn, c, 0)
    KeyEvent                 (fromEnum' -> c) (fromEnum' -> v) -> (fromEnum' EvKey, c, v)
    RelativeEvent            (fromEnum' -> c) (fromEnum' -> v) -> (fromEnum' EvRel, c, v)
    AbsoluteEvent            (fromEnum' -> c) (fromEnum' -> v) -> (fromEnum' EvAbs, c, v)
    MiscEvent                (fromEnum' -> c) (fromEnum' -> v) -> (fromEnum' EvMsc, c, v)
    SwitchEvent              (fromEnum' -> c) (fromEnum' -> v) -> (fromEnum' EvSw,  c, v)
    LEDEvent                 (fromEnum' -> c) (fromEnum' -> v) -> (fromEnum' EvLed, c, v)
    SoundEvent               (fromEnum' -> c) (fromEnum' -> v) -> (fromEnum' EvSnd, c, v)
    RepeatEvent              (fromEnum' -> c) (fromEnum' -> v) -> (fromEnum' EvRep, c, v)
    ForceFeedbackEvent       (fromEnum' -> c) (fromEnum' -> v) -> (fromEnum' EvFf,  c, v)
    PowerEvent               (fromEnum' -> c) (fromEnum' -> v) -> (fromEnum' EvPwr, c, v)
    ForceFeedbackStatusEvent (fromEnum' -> c) (fromEnum' -> v) -> (fromEnum' EvFfStatus, c, v)
    UnknownEvent             (fromEnum' -> t) (fromEnum' -> c) (fromEnum' -> v) -> (t, c, v)

fromCTimeVal :: Raw.Timeval -> DiffTime
fromCTimeVal Raw.Timeval{tv_sec = s, tv_usec = us} =
    fromRational $ fromIntegral s + (fromIntegral us % 1_000_000)

--TODO QuickCheck inverse
toCTimeVal :: DiffTime -> Raw.Timeval
toCTimeVal t = Raw.Timeval n (round $ f * 1_000_000)
    where (n,f) = properFraction t

{- | Create a device from a valid path - usually /\/dev\/input\/eventX/ for some numeric /X/.
Use 'newDeviceFromFd' if you need more control over how the device is created.
-}
newDevice :: RawFilePath -> IO Device
newDevice path = newDeviceFromFd =<< openFd path ReadWrite defaultFileFlags

{- | Generalisation of 'newDevice', in case one needs control over the file descriptor,
e.g. in order to set a particular 'System.Posix.FileMode', 'System.Posix.OpenMode', or 'System.Posix.OpenFileFlags'.
Note that:

> newDevice path = newDeviceFromFd =<< openFd path ReadOnly Nothing defaultFileFlags

__WARNING__: Don't attempt to reuse the 'Fd' - it will be closed when the 'Device' is garbage collected.
-}
newDeviceFromFd :: Fd -> IO Device
newDeviceFromFd fd = do
    dev <- cErrCall "newDeviceFromFd" () $ LL.newDeviceFromFd fd
    pid <- getProcessID
    path <- readSymbolicLink $ "/proc/" <> show pid <> "/fd/" <> show fd
    return $ Device{cDevice = dev, devicePath = pack path}

-- | The usual directory containing devices (/"\/dev\/input"/).
evdevDir :: RawFilePath
evdevDir = "/dev/input"

deviceName :: Device -> IO ByteString
deviceName = join . LL.deviceName . cDevice

deviceFd :: Device -> IO Fd
deviceFd = LL.deviceFd . cDevice
devicePhys :: Device -> IO (Maybe ByteString)
devicePhys = join . LL.devicePhys . cDevice
deviceUniq :: Device -> IO (Maybe ByteString)
deviceUniq = join . LL.deviceUniq . cDevice
deviceProduct :: Device -> IO Int
deviceProduct = LL.deviceProduct . cDevice
deviceVendor :: Device -> IO Int
deviceVendor = LL.deviceVendor . cDevice
deviceBustype :: Device -> IO Int
deviceBustype = LL.deviceBustype . cDevice
deviceVersion :: Device -> IO Int
deviceVersion = LL.deviceVersion . cDevice

deviceProperties :: Device -> IO [DeviceProperty]
deviceProperties dev = filterM (LL.hasProperty $ cDevice dev) enumerate

deviceEventTypes :: Device -> IO [EventType]
deviceEventTypes dev = filterM (LL.hasEventType $ cDevice dev) enumerate

--TODO this is an imperfect API since '_val' is ignored entirely
deviceHasEvent :: Device -> EventData -> IO Bool
deviceHasEvent dev e = LL.hasEventCode (cDevice dev) typ code
  where (typ,code,_val) = toCEventData e

data AbsInfo = AbsInfo
    { absValue :: Int32
    , absMinimum :: Int32
    , absMaximum :: Int32
    , absFuzz :: Int32
    , absFlat :: Int32
    , absResolution :: Int32
    }
    deriving (Show)

deviceAbsAxis :: Device -> AbsoluteAxis -> IO (Maybe AbsInfo)
deviceAbsAxis dev (fromEnum' -> code) = withForeignPtr (cDevice dev) \devPtr ->
    (unConstPtr <$> Raw.libevdev_get_abs_info (ConstPtr devPtr) (CUInt code))
        >>= handleNull (pure Nothing) \absInfoPtr ->
            peek absInfoPtr <&> \raw ->
                Just
                    AbsInfo
                        { absValue = coerce raw.value
                        , absMinimum = coerce raw.minimum
                        , absMaximum = coerce raw.maximum
                        , absFuzz = coerce raw.fuzz
                        , absFlat = coerce raw.flat
                        , absResolution = coerce raw.resolution
                        }

data LEDValue = LedOn | LedOff
    deriving (Bounded, Eq, Ord, Read, Show)

-- | Set the state of a LED on a device.
setDeviceLED :: Device -> LEDEvent -> LEDValue -> IO ()
setDeviceLED dev led val = cErrCall "setDeviceLED" dev $ withForeignPtr (cDevice dev) \devPtr ->
    Errno <$> Raw.libevdev_kernel_set_led_value devPtr (fromEnum' led) case val of
        LedOn -> Raw.LIBEVDEV_LED_ON
        LedOff -> Raw.LIBEVDEV_LED_OFF

{- Util -}

grabDevice' :: Raw.Libevdev_grab_mode -> Device -> IO ()
grabDevice' mode dev = cErrCall "grabDevice" dev $
    withForeignPtr (cDevice dev) $ fmap Errno . flip Raw.libevdev_grab mode

{-
TODO this is a workaround until c2hs has a better story for enum conversions
    when we remove it we can get rid of '-fno-state-hack'

based on profiling, and Debug.Trace, it seems that 'enumMap' is computed no more times than necessary
    (6 - number of combinations of a and k that it is called with)
    but based on https://www.reddit.com/r/haskell/comments/grskne/help_reasoning_about_performance_memoization/,
        it's possible that behaviour is worse without profiling on (argh...)

open c2hs issue
    we perhaps essentially want the `CEnum` class proposed at: https://github.com/haskell/c2hs/issues/78
        but perhaps belonging (at least initially) in c2hs rather than base, for expediency
        this doesn't necessarily consider enum defines though - discussion is around capturing the semantics of actual C enums
    alternatively, monomorphic functions for each type, as with c2hs's with* functions
-}
toEnum' :: forall k a. (Ord k, Enum k, Bounded a, Enum a) => k -> Maybe a
toEnum' = (enumMap !?)
  where
    --TODO HashMap, IntMap?
    enumMap :: Map k a
    enumMap = Map.fromList $ map (toEnum . fromEnum &&& id) enumerate

instance CErrInfo Device where
    cErrInfo = return . Just . devicePath
