{-# OPTIONS_GHC -fno-state-hack #-}
module Evdev (
    defaultReadFlags,
    grabDevice,
    ungrabDevice,
    nextEvent,
    newDevice,
    evdevDir,
    deviceName,
    deviceFd,
    devicePath,
    deviceProperties,
    Device,
    Event(..),
    EventData(..),
    LL.CEvent(..),
    LL.CTimeVal(..),
    toCEvent,
    fromCEvent,
    EventCode(..),
    EventValue(..),
    KeyEventType(..),
    LL.ReadFlag(..),
) where

import Control.Arrow ((&&&))
import Control.Monad (filterM,join)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int32)
import Data.List.Extra (enumerate)
import Data.Map ((!?), Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (DiffTime)
import Data.Word (Word16)
import Foreign ((.|.))
import Foreign.C (CUInt)
import Foreign.C.Error (Errno(Errno),errnoToIOError)
import System.Posix.ByteString (Fd,RawFilePath)
import System.Posix.IO.ByteString (fdToHandle)

import qualified Evdev.LowLevel as LL
import Evdev.Codes

-- stores path that was originally used, as it seems impossible to recover this later
-- We don't allow the user to access the underlying low-level C device.
data Device = Device { cDevice :: LL.Device, devicePath :: RawFilePath }
instance Show Device where
    show = show . devicePath

data Event = Event
    { eventData :: EventData
    , eventTime :: DiffTime
    }
    deriving (Eq, Ord, Show)

--TODO name?
data EventData
    = SyncEvent SyncEventType
    | KeyEvent Key KeyEventType
    | RelativeEvent RelativeAxis EventValue
    | AbsoluteEvent AbsoluteAxis EventValue
    | MiscEvent MiscEventType EventValue
    | SwitchEvent SwitchEventType EventValue
    | LEDEvent LEDEventType EventValue
    | SoundEvent SoundEventType EventValue
    | RepeatEvent RepeatEventType EventValue
    | ForceFeedbackEvent EventCode EventValue
    | PowerEvent EventCode EventValue
    | ForceFeedbackStatusEvent EventCode EventValue
    | UnknownEvent Word16 EventCode EventValue {- ^ We include this primarily so that 'fromCEvent' can be well-defined -
        let us know if you ever actually see one emitted by a device, as it would likely
        indicate a shortcoming in the library. -}
    deriving (Eq, Ord, Read, Show)

newtype EventCode = EventCode Word16
    deriving stock (Eq, Ord, Read, Show)
    deriving newtype (Enum, Integral, Real, Num) --TODO all this baggage to make 'toEnum'' slightly easier?
newtype EventValue = EventValue Int32
    deriving stock (Eq, Ord, Read, Show)
    deriving newtype (Enum, Integral, Real, Num)

data KeyEventType
    = Released
    | Pressed
    | Repeated
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

convertFlags :: Set LL.ReadFlag -> CUInt
convertFlags = fromIntegral . foldr ((.|.) . fromEnum) 0

defaultReadFlags :: Set LL.ReadFlag
defaultReadFlags = Set.fromList [LL.Normal, LL.Blocking]

grabDevice :: Device -> IO ()
grabDevice = grabDevice' LL.LibevdevGrab
ungrabDevice :: Device -> IO ()
ungrabDevice = grabDevice' LL.LibevdevUngrab

nextEvent :: Device -> Set LL.ReadFlag -> IO Event
nextEvent dev flags =
    fromCEvent <$> throwCErrors "nextEvent" (Right dev) (LL.nextEvent (cDevice dev) (convertFlags flags))

fromCEvent :: LL.CEvent -> Event
fromCEvent (LL.CEvent t (EventCode -> c) (EventValue -> v) time) = Event event $ fromCTimeVal time
  where
    event = fromMaybe unknown $ toEnum' t >>= \case
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
    unknown = UnknownEvent t c v

toCEvent :: Event -> LL.CEvent
toCEvent (Event e time) = case e of
    -- from kernel docs, 'EV_SYN event values are undefined' - we always seem to see 0, so may as well use that
    SyncEvent                (fe -> c) -> LL.CEvent (fe EvSyn) c 0 cTime
    KeyEvent                 (fe -> c) (fe -> v) -> LL.CEvent (fe EvKey) c v cTime
    RelativeEvent            (fe -> c) (fe -> v) -> LL.CEvent (fe EvRel) c v cTime
    AbsoluteEvent            (fe -> c) (fe -> v) -> LL.CEvent (fe EvAbs) c v cTime
    MiscEvent                (fe -> c) (fe -> v) -> LL.CEvent (fe EvMsc) c v cTime
    SwitchEvent              (fe -> c) (fe -> v) -> LL.CEvent (fe EvSw)  c v cTime
    LEDEvent                 (fe -> c) (fe -> v) -> LL.CEvent (fe EvLed) c v cTime
    SoundEvent               (fe -> c) (fe -> v) -> LL.CEvent (fe EvSnd) c v cTime
    RepeatEvent              (fe -> c) (fe -> v) -> LL.CEvent (fe EvRep) c v cTime
    ForceFeedbackEvent       (fe -> c) (fe -> v) -> LL.CEvent (fe EvFf)  c v cTime
    PowerEvent               (fe -> c) (fe -> v) -> LL.CEvent (fe EvPwr) c v cTime
    ForceFeedbackStatusEvent (fe -> c) (fe -> v) -> LL.CEvent (fe EvFfStatus) c v cTime
    UnknownEvent             (fe -> t) (fe -> c) (fe -> v) -> LL.CEvent t c v cTime
  where
    --TODO this isn't entirely safe in general, though it's really no worse than 'fromEnum'
    -- if we could tell C2HS which int type each #defined enum corresponded to, we could check this statically
    fe :: (Enum a, Integral b) => a -> b
    fe = fromIntegral . fromEnum
    cTime = toCTimeVal time

newDevice :: RawFilePath -> IO Device
newDevice path = do
    dev <- throwCErrors "newDevice" (Left path) $ LL.newDevice path
    return $ Device dev path

evdevDir :: RawFilePath
evdevDir = "/dev/input"

deviceName :: Device -> IO ByteString
deviceName = join . LL.deviceName . cDevice

deviceFd :: Device -> IO Fd
deviceFd = LL.deviceFd . cDevice

deviceProperties :: Device -> IO [DeviceProperty]
deviceProperties dev = filterM (LL.hasProperty $ cDevice dev) enumerate


{- Util -}

-- run the action, throwing a relevant exception if the C errno is not 0
throwCErrors :: String -> Either ByteString Device -> IO (Errno, a) -> IO a
throwCErrors func pathOrDev x = do
    (errno, res) <- x
    case errno of
        Errno 0 -> return res
        Errno n -> do
            (handle,path) <- case pathOrDev of
                Left path -> return (Nothing,path)
                Right dev -> do
                    h <- fdToHandle =<< deviceFd dev
                    return (Just h, devicePath dev)
            ioError $ errnoToIOError func (Errno $ abs n) handle (Just $ BS.unpack path)

grabDevice' :: LL.GrabMode -> Device -> IO ()
grabDevice' mode dev = throwCErrors "grabDevice" (Right dev) $ LL.grabDevice (cDevice dev) mode

--TODO QuickCheck inverse
toCTimeVal :: DiffTime -> LL.CTimeVal
toCTimeVal t = LL.CTimeVal n (round $ f * 1_000_000)
    where (n,f) = properFraction t
fromCTimeVal :: LL.CTimeVal -> DiffTime
fromCTimeVal (LL.CTimeVal s us) =
    fromRational $ fromIntegral s + (fromIntegral us % 1_000_000)

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
toEnum' :: forall k a. (Integral k, Bounded a, Enum a) => k -> Maybe a
toEnum' = (enumMap !?)
  where
    --TODO HashMap, IntMap?
    enumMap :: Map k a
    enumMap = Map.fromList $ map (fromIntegral . fromEnum &&& id) enumerate
