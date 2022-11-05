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
    LL.AbsInfo (..),
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
    LL.LEDValue(..),
    setDeviceLED,
    -- ** C-style types
    -- | These correspond more directly to C's /input_event/ and /timeval/.
    -- They are used internally, but may be useful for advanced users.
    LL.CEvent(..),
    toCEvent,
    fromCEvent,
    toCEventData,
    fromCEventData,
    LL.CTimeVal(..),
    toCTimeVal,
    fromCTimeVal,
) where

import Control.Arrow ((&&&))
import Control.Monad (filterM, join)
import Data.ByteString.Char8 (ByteString, pack)
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
import Foreign ((.|.))
import Foreign.C (CUInt)
import System.Posix.Process (getProcessID)
import System.Posix.Files (readSymbolicLink)
import System.Posix.ByteString (Fd, RawFilePath)
import System.Posix.IO.ByteString (OpenMode (..), defaultFileFlags, openFd)

import qualified Evdev.LowLevel as LL
import Evdev.Codes
import Util

-- stores path that was originally used, as it seems impossible to recover this later
-- We don't allow the user to access the underlying low-level C device.
-- | An input device.
data Device = Device { cDevice :: LL.Device, devicePath :: ByteString }


instance Show Device where
    show = show . devicePath

-- | An input event, including the timestamp.
data Event = Event
    { eventData :: EventData
    , eventTime :: DiffTime
    }
    deriving (Eq, Ord, Show)

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
    deriving (Eq, Ord, Show)

-- | A direct representation of the /code/ field of the C /input_event/, for when there is no obvious meaningful sum type.
newtype EventCode = EventCode Word16
    deriving (Eq, Ord, Show, Enum)
-- | A direct representation of the /value/ field of the C /input_event/, for when there is no obvious meaningful sum type.
newtype EventValue = EventValue Int32
    deriving (Eq, Ord, Show, Enum)

-- | The status of a key.
data KeyEvent
    = Released
    | Pressed
    | Repeated
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

convertFlags :: Set LL.ReadFlag -> CUInt
convertFlags = fromIntegral . foldr ((.|.) . fromEnum) 0

defaultReadFlags :: Set LL.ReadFlag
defaultReadFlags = Set.fromList [LL.Normal, LL.Blocking]

nonBlockingReadFlags :: Set LL.ReadFlag
nonBlockingReadFlags = Set.fromList [LL.Normal]

-- | Prevent other clients (including kernel-internal ones) from receiving events. Often a bad idea.
grabDevice :: Device -> IO ()
grabDevice = grabDevice' LL.LibevdevGrab
-- | Release a grabbed device.
ungrabDevice :: Device -> IO ()
ungrabDevice = grabDevice' LL.LibevdevUngrab

-- | Get the next event from the device.
nextEvent :: Device -> IO Event
nextEvent dev =
    fromCEvent <$> cErrCall "nextEvent" dev (LL.nextEvent (cDevice dev) (convertFlags defaultReadFlags))

{- | Get the next event from the device, if one is available.
Designed for use with devices created from a non-blocking file descriptor. Otherwise equal to @fmap Just . nextEvent@.
-}
nextEventMay :: Device -> IO (Maybe Event)
nextEventMay dev =
    fmap fromCEvent <$> cErrCall "nextEventMay" dev (LL.nextEventMay (cDevice dev) (convertFlags nonBlockingReadFlags))

fromCEvent :: LL.CEvent -> Event
fromCEvent (LL.CEvent t c v time) = Event (fromCEventData (t,c,v)) $ fromCTimeVal time

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

toCEvent :: Event -> LL.CEvent
toCEvent (Event e time) = uncurry3 LL.CEvent (toCEventData e) $ toCTimeVal time

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

fromCTimeVal :: LL.CTimeVal -> DiffTime
fromCTimeVal (LL.CTimeVal s us) =
    fromRational $ fromIntegral s + (fromIntegral us % 1_000_000)

--TODO QuickCheck inverse
toCTimeVal :: DiffTime -> LL.CTimeVal
toCTimeVal t = LL.CTimeVal n (round $ f * 1_000_000)
    where (n,f) = properFraction t

{- | Create a device from a valid path - usually /\/dev\/input\/eventX/ for some numeric /X/.
Use 'newDeviceFromFd' if you need more control over how the device is created.
-}
newDevice :: RawFilePath -> IO Device
newDevice path = newDeviceFromFd =<< openFd path ReadWrite Nothing defaultFileFlags

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

deviceAbsAxis :: Device -> AbsoluteAxis -> IO (Maybe LL.AbsInfo)
deviceAbsAxis dev = LL.getAbsInfo (cDevice dev) . fromEnum'

-- | Set the state of a LED on a device.
setDeviceLED :: Device -> LEDEvent -> LL.LEDValue -> IO ()
setDeviceLED dev led val = cErrCall "setDeviceLED" dev (LL.libevdev_kernel_set_led_value (cDevice dev) led val)

{- Util -}

grabDevice' :: LL.GrabMode -> Device -> IO ()
grabDevice' mode dev = cErrCall "grabDevice" dev $
    LL.grabDevice (cDevice dev) mode

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
