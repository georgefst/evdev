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
    evdevDir,
    deviceName,
    deviceFd,
    devicePath,
    deviceProperties,
    Device,
    Event(..), --TODO provide access to Word16 etc...
    EventCode(..),
    EventValue(..),
    KeyEventType(..),
    ReadFlag(..),
) where

import Control.Arrow (second)
import Control.Monad (filterM)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int32)
import Data.List.Extra (enumerate)
import Data.Set (Set)
import Data.Time.Clock (DiffTime,picosecondsToDiffTime)
import Data.Word (Word16)
import Foreign ((.|.))
import Foreign.C (CInt(..),CUInt(..),CUShort(..))
import Foreign.C.Error (Errno(Errno),errnoToIOError)
import Safe (initSafe,tailSafe)
import System.Posix.ByteString (Fd,RawFilePath)
import System.Posix.IO.ByteString (fdToHandle)

import qualified Evdev.LowLevel as LL
import Evdev.LowLevel (ReadFlag(..))
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
pattern SyncEvent c <- Event EvSyn (convertEnum -> c) _ _

pattern KeyEvent :: Key -> KeyEventType -> Event
pattern KeyEvent c v <- Event EvKey (convertEnum -> c) (convertEnum -> v) _

pattern RelativeEvent :: RelativeAxis -> EventValue -> Event
pattern RelativeEvent c v <- Event EvRel (convertEnum -> c) v _

pattern AbsoluteEvent :: AbsoluteAxis -> EventValue -> Event
pattern AbsoluteEvent c v <- Event EvAbs (convertEnum -> c) v _

pattern MiscEvent :: MiscEventType -> EventValue -> Event
pattern MiscEvent c v <- Event EvMsc (convertEnum -> c) v _

pattern SwitchEvent :: SwitchEventType -> EventValue -> Event
pattern SwitchEvent c v <- Event EvSw (convertEnum -> c) v _

pattern LEDEvent :: LEDEventType -> EventValue -> Event
pattern LEDEvent c v <- Event EvLed (convertEnum -> c) v _

pattern SoundEvent :: SoundEventType -> EventValue -> Event
pattern SoundEvent c v <- Event EvSnd (convertEnum -> c) v _

pattern RepeatEvent :: RepeatEventType -> EventValue -> Event
pattern RepeatEvent c v <- Event EvRep (convertEnum -> c) v _

pattern ForceFeedbackEvent :: EventCode -> EventValue -> Event
pattern ForceFeedbackEvent c v <- Event EvFf c v _

pattern PowerEvent :: EventCode -> EventValue -> Event
pattern PowerEvent c v <- Event EvPwr c v _

pattern ForceFeedbackStatusEvent :: EventCode -> EventValue -> Event
pattern ForceFeedbackStatusEvent c v <- Event EvFfStatus c v _

newtype EventCode = EventCode Word16 deriving (Enum, Eq, Ord, Read, Show)
newtype EventValue = EventValue Int32 deriving (Enum, Eq, Ord, Read, Show)

data KeyEventType
    = Released
    | Pressed
    | Repeated
    deriving (Enum, Eq, Ord, Read, Show)

convertFlags :: Set ReadFlag -> CUInt
convertFlags = fromIntegral . foldr ((.|.) . fromEnum) 0

defaultReadFlags :: Set ReadFlag
defaultReadFlags = [Normal,Blocking]

grabDevice :: Device -> IO ()
grabDevice = grabDevice' LL.LibevdevGrab
ungrabDevice :: Device -> IO ()
ungrabDevice = grabDevice' LL.LibevdevUngrab

nextEvent :: Device -> Set ReadFlag -> IO Event
nextEvent dev flags = do
    (CUShort t, CUShort c, CInt v, s, us) <-
        throwCErrors "nextEvent" (Right dev) $ LL.nextEvent (cDevice dev) (convertFlags flags)
    return $ Event (convertEnum t) (EventCode c) (EventValue v) $
        picosecondsToDiffTime $ 1_000_000_000_000 * fromIntegral s + 1_000_000 * fromIntegral us

newDevice :: RawFilePath -> IO Device
newDevice path = do
    dev <- throwCErrors "newDevice" (Left path) $ LL.newDevice path
    return $ Device dev path

evdevDir :: RawFilePath
evdevDir = "/dev/input"

deviceName :: Device -> IO ByteString
deviceName = fmap BS.pack . LL.deviceName . cDevice

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

--TODO ensure all uses are safe
    -- really, fromEnum should be :: e -> Integer
    -- and toEnum :: Integral a => a -> Maybe e
    -- the issues with Enum are great enough that it may be worth considering using something more bespoke than c2hs
    -- or it could be worth a PR
        -- auto-generate safe *toInt/*fromInt based on the current logic
        -- then implement Enum instance in terms of those
    -- note that we also use to/from-Enum in LowLevel
-- obviously this isn't safe in general
-- we use it only after matching on 'EventType', to get the corresponding 'EventCode' and 'EventValue'
convertEnum :: (Enum a, Enum b) => a -> b
convertEnum = toEnum . fromEnum
