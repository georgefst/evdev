module Evdev (
    pattern SyncEvent,
    pattern KeyEvent,
    pattern RelativeEvent,
    pattern AbsoluteEvent,
    pattern MiscEvent,
    pattern SwitchEvent,
    pattern LEDEvent,
    pattern SoundEvent,
    pattern RepeatEvent,
    pattern ForceFeedbackEvent,
    pattern PowerEvent,
    pattern ForceFeedbackStatusEvent,
    prettyEvent,
    defaultReadFlags,
    grabDevice,
    ungrabDevice,
    nextEvent,
    newDevice,
    maybeNewDevice,
    evdevDir,
    getDeviceName,
    Device (devicePath),
    Event,
    EventCode(..),
    EventValue(..),
    KeyEventType(..),
    ReadFlags (..),
) where

import Control.Exception
import Data.Int
import Data.Either.Combinators
import Data.Time.Clock
import Data.Tuple.Extra
import Safe

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Set (Set)
import Foreign ((.|.))
import Foreign.C (CUInt)
import Foreign.C.Error (Errno(Errno),errnoToIOError)
import System.Posix.ByteString (RawFilePath)

import qualified Evdev.LowLevel as LL
import Evdev.LowLevel (ReadFlags(..))
import Evdev.Codes

-- stores path that was originally used, as it seems impossible to recover this later
-- We don't allow the user to access the underlying low-level C device.
data Device = Device { cDevice :: LL.Device, devicePath :: RawFilePath }
instance Show Device where
    show = show . devicePath

data Event = Event {
    evType :: EventType,
    evCode :: EventCode,
    evValue :: EventValue,
    evTime :: DiffTime}
    deriving (Eq, Ord, Show)

-- aligns with the pattern synonyms below
prettyEvent :: Event -> String
prettyEvent x = showTime (evTime x) ++ ":" ++ " " ++ case x of
        SyncEvent t -> show t
        KeyEvent k t -> unwords [show k, show t]
        RelativeEvent c v -> unwords [show c, showE v]
        AbsoluteEvent c v -> unwords [show c, showE v]
        MiscEvent c v -> unwords [show c, showE v]
        SwitchEvent c v -> unwords [show c, showE v]
        LEDEvent c v -> unwords [show c, showE v]
        SoundEvent c v -> unwords [show c, showE v]
        RepeatEvent c v -> unwords [show c, showE v]
        ForceFeedbackEvent c v -> unwords [showE c, showE v]
        PowerEvent c v -> unwords [showE c, showE v]
        ForceFeedbackStatusEvent c v -> unwords [showE c, showE v]
        _ -> error $ "show: unrecognised Event: " ++ unwords
            [showE $ evType x, showE $ evCode x, showE $ evValue x]
        where
            showE :: Enum x => x -> String
            showE = show . fromEnum
            showTime t = -- fix time string to always have same length after '.', by adding 0s
                let (n,r) = second tailSafe $ span (/= '.') $ initSafe $ show t
                in  n ++ "." ++ take 6 (r ++ ['0'..]) ++ "s"

pattern SyncEvent :: SyncEventType -> Event
pattern SyncEvent c <- Event EvSyn (toEnum . fromEnum -> c) _ _

pattern KeyEvent :: Key -> KeyEventType -> Event
pattern KeyEvent c v <- Event EvKey (toEnum . fromEnum -> c) (toEnum . fromEnum -> v) _

pattern RelativeEvent :: RelativeAxis -> EventValue -> Event
pattern RelativeEvent c v <- Event EvRel (toEnum . fromEnum -> c) v _

pattern AbsoluteEvent :: AbsoluteAxis -> EventValue -> Event
pattern AbsoluteEvent c v <- Event EvAbs (toEnum . fromEnum -> c) v _

pattern MiscEvent :: MiscEventType -> EventValue -> Event
pattern MiscEvent c v <- Event EvMsc (toEnum . fromEnum -> c) v _

pattern SwitchEvent :: SwitchEventType -> EventValue -> Event
pattern SwitchEvent c v <- Event EvSw (toEnum . fromEnum -> c) v _

pattern LEDEvent :: LEDEventType -> EventValue -> Event
pattern LEDEvent c v <- Event EvLed (toEnum . fromEnum -> c) v _

pattern SoundEvent :: SoundEventType -> EventValue -> Event
pattern SoundEvent c v <- Event EvSnd (toEnum . fromEnum -> c) v _

pattern RepeatEvent :: RepeatEventType -> EventValue -> Event
pattern RepeatEvent c v <- Event EvRep (toEnum . fromEnum -> c) v _

pattern ForceFeedbackEvent :: EventCode -> EventValue -> Event
pattern ForceFeedbackEvent c v <- Event EvFf c v _

pattern PowerEvent :: EventCode -> EventValue -> Event
pattern PowerEvent c v <- Event EvPwr c v _

pattern ForceFeedbackStatusEvent :: EventCode -> EventValue -> Event
pattern ForceFeedbackStatusEvent c v <- Event EvFfStatus c v _

newtype EventCode = EventCode Int16 deriving (Enum, Eq, Ord, Read, Show)
newtype EventValue = EventValue Int32 deriving (Enum, Eq, Ord, Read, Show)

data KeyEventType
    = Released
    | Pressed
    | Repeated
    deriving (Enum, Eq, Ord, Read, Show)

convertFlags :: Set ReadFlags -> CUInt
convertFlags = fromIntegral . foldr ((.|.) . fromEnum) 0

defaultReadFlags :: Set ReadFlags
defaultReadFlags = [Normal,Blocking]

grabDevice :: Device -> IO ()
grabDevice = grabDevice' LL.LibevdevGrab
ungrabDevice :: Device -> IO ()
ungrabDevice = grabDevice' LL.LibevdevUngrab

nextEvent :: Device -> Set ReadFlags -> IO Event
nextEvent dev flags = do
    (t,c,v,time) <- LL.convertEvent =<<
        throwCErrors "nextEvent" (devicePath dev) (LL.nextEvent (cDevice dev) (convertFlags flags))
    return $ Event (toEnum t) (EventCode c) (EventValue v) time

newDevice :: RawFilePath -> IO Device
newDevice path = do
    dev <- throwCErrors "newDevice" path $ LL.newDevice path
    return $ Device dev path

maybeNewDevice :: RawFilePath -> IO (Maybe Device)
maybeNewDevice = fmap rightToMaybe . tryIO . newDevice

evdevDir :: RawFilePath
evdevDir = "/dev/input"

getDeviceName :: Device -> IO ByteString
getDeviceName = fmap BS.pack . LL.deviceName . cDevice


{- Util -}

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

-- run the action, throwing an error if the C errno is not 0
throwCErrors :: String -> RawFilePath -> IO (Errno, a) -> IO a
throwCErrors loc path x = do
    (errno, res) <- x
    case errno of
        Errno 0 -> return res
        _ -> ioError $ errnoToIOError loc errno Nothing (Just $ BS.unpack path)

grabDevice' :: LL.GrabMode -> Device -> IO ()
grabDevice' mode dev = throwCErrors "grabDevice" (devicePath dev) $ LL.grabDevice (cDevice dev) mode
