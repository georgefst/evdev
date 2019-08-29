{-# LANGUAGE ForeignFunctionInterface #-}

module Evdev (
      Device
    , Event (..)
    , EventStream
    , Fd (..)
    , Key
    , KeyEventType (..)
    , SyncEventType (..)
    , defaultReadFlags
    , devicePath
    , getDeviceName
    , getDeviceProperties
    , grabDevice
    , newDevice
    , nextEvent
    , readEvents
    , runOnEvents
    , ungrabDevice
) where

import Control.Applicative
import Control.Concurrent
import Control.Exception.Extra
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Composition
import Data.Either
import Data.Int
import Data.List.Extra
import Data.Maybe
import Data.Time.Clock

import qualified Data.ByteString.Char8 as BS
import Data.Set (Set)
import Foreign (Ptr,(.|.))
import Foreign.C (CInt,CUInt,CLong)
import GHC.IO.Exception (IOException(IOError),IOErrorType(IllegalOperation,NoSuchThing,PermissionDenied),ioe_type)
import Protolude.Functor ((<<$>>))
import Protolude.Applicative (liftAA2)
import RawFilePath.Directory (doesFileExist,listDirectory)
import Streaming.Prelude (Of,Stream)
import qualified Streaming.Prelude as Streaming
import System.INotify (addWatch,initINotify)
import qualified System.INotify as INotify
import System.Posix.ByteString (RawFilePath)
import System.Posix.FilePath ((</>),combine)
import System.Posix.IO.ByteString (OpenMode(ReadOnly),defaultFileFlags,openFd)
import System.Posix.Types (Fd(Fd))

import Evdev.Codes

#include <libevdev-1.0/libevdev/libevdev.h>
#include <linux/input.h>


{- Exported types -}

-- We don't allow the user to access the underlying CDevice.
data Device
    = Device { cDevice :: CDevice, devicePath :: RawFilePath }

data Event
    = SyncEvent SyncEventType DiffTime
    | KeyEvent Key KeyEventType DiffTime
    | RelativeEvent RelativeAxis EventValue DiffTime
    | AbsoluteEvent AbsoluteAxis EventValue DiffTime
    | MiscEvent MiscEventType EventValue DiffTime
    | SwitchEvent SwitchEventType EventValue DiffTime
    | LEDEvent LEDEventType EventValue DiffTime
    | SoundEvent SoundEventType EventValue DiffTime
    | AutorepeatEvent AutorepeatEventType EventValue DiffTime
    | FFEvent EventCode EventValue DiffTime
    | PowerEvent EventCode EventValue DiffTime
    | FFStatusEvent EventCode EventValue DiffTime
    | UnknownEvent EventType EventCode EventValue DiffTime -- for forward compatibility
    deriving (Eq, Show)
eventTime :: Event -> DiffTime
eventTime = \case
    KeyEvent _ _ t -> t
    RelativeEvent _ _ t -> t
    AbsoluteEvent _ _ t -> t
    MiscEvent _ _ t -> t
    SwitchEvent _ _ t -> t
    SyncEvent _ t -> t
    LEDEvent _ _ t -> t
    SoundEvent _ _ t -> t
    AutorepeatEvent _ _ t -> t
    FFEvent _ _ t -> t
    PowerEvent _ _ t -> t
    FFStatusEvent _ _ t -> t
    UnknownEvent _ _ _ t -> t

data KeyEventType
    = Released
    | Pressed
    | Repeated
    deriving (Enum, Eq, Ord, Read, Show)

{#enum libevdev_read_flag as ReadFlags {
    LIBEVDEV_READ_FLAG_SYNC as Sync,
    LIBEVDEV_READ_FLAG_NORMAL as Normal,
    LIBEVDEV_READ_FLAG_FORCE_SYNC as ForceSync,
    LIBEVDEV_READ_FLAG_BLOCKING as Blocking }
    deriving (Eq,Ord,Show) #}

-- TODO safe to derive integral, real?
newtype EventType = EventType Int16 deriving (Enum, Eq, Ord, Read, Show)
newtype EventCode = EventCode Int16 deriving (Enum, Eq, Ord, Read, Show)
newtype EventValue = EventValue Int32 deriving (Enum, Eq, Ord, Read, Show)


{- Low-level types -}

{#pointer *input_event as CEvent foreign newtype#}

{#pointer *timeval as CTime foreign newtype#}

{#pointer *libevdev as CDevice newtype #}

{#enum libevdev_grab_mode as GrabMode { underscoreToCase } deriving (Show) #}


{- Conversions -}

convertEvent :: CEvent -> IO Event
convertEvent ev =
    join $ classifyEvent
        <$> fmap EventType (getIntField {#get input_event->type #})
        <*> fmap EventCode (getIntField {#get input_event->code #})
        <*> fmap EventValue (getIntField {#get input_event->value #})
        <*> withCEvent ev getTime
    where
        getIntField :: (Integral a,Integral b) => (Ptr CEvent -> IO a) -> IO b
        getIntField f = withCEvent ev (fmap fromIntegral . f)
        getTime :: Ptr CEvent -> IO DiffTime
        getTime ptr =
            let sec, usec :: IO CLong
                sec = C2HSImp.peekByteOff ptr 0
                usec = C2HSImp.peekByteOff ptr {#sizeof __kernel_time_t #}
            in  convertTime <$> (fromIntegral <$> sec) <*> (fromIntegral <$> usec)
        convertTime s us = picosecondsToDiffTime $ 1000000000000 * fromIntegral s + 1000000 * fromIntegral us
        toEnumSafe :: Enum a => Int -> IO (Maybe a) -- little bit of a hack
        toEnumSafe = fmap rightToMaybe . (try :: IO a -> IO (Either ErrorCall a)) . evaluate . toEnum
        convertEnumSafe :: (Enum a, Enum b) => a -> IO (Maybe b)
        convertEnumSafe = toEnumSafe . fromEnum
        classifyEvent :: EventType -> EventCode -> EventValue -> DiffTime -> IO Event
        classifyEvent t@(EventType t') c v ts = ($ ts) . fromMaybe defaultEvent <$> case t' of
            {#const EV_SYN #} -> SyncEvent <<$>> convertEnumSafe c
            {#const EV_KEY #} -> liftAA2 KeyEvent (convertEnumSafe c) (convertEnumSafe v)
            {#const EV_ABS #} -> simple AbsoluteEvent
            {#const EV_REL #} -> simple RelativeEvent
            {#const EV_MSC #} -> simple MiscEvent
            {#const EV_SW #}  -> simple SwitchEvent
            {#const EV_LED #} -> simple LEDEvent
            {#const EV_SND #} -> simple SoundEvent
            {#const EV_REP #} -> simple AutorepeatEvent
            {#const EV_FF #} -> simple FFEvent
            {#const EV_PWR #} -> simple PowerEvent
            {#const EV_FF_STATUS #} -> simple FFStatusEvent
            _ -> pure $ pure $ defaultEvent
            where
                defaultEvent :: DiffTime -> Event
                defaultEvent = UnknownEvent t c v
                simple :: Enum a => (a -> EventValue -> DiffTime -> Event) -> IO (Maybe (DiffTime -> Event))
                simple x = liftAA2 x (convertEnumSafe c) (pure $ pure $ v)

convertFlags :: Set ReadFlags -> CUInt
convertFlags = fromIntegral . foldr ((.|.).fromEnum) 0

convertEnum :: (Enum a, Integral b) => a -> b
convertEnum = fromIntegral . fromEnum


{- Exported functions -}

-- blocking seems to be necessary to stop us from only getting half the events
defaultReadFlags :: Set ReadFlags
defaultReadFlags = [Normal,Blocking]

-- unofrtunately it seems impossible to use '+' without discarding the original output data
-- so we have to do this bit manually
-- TODO PR on c2hs to add this functionality
-- {#fun libevdev_next_event as ^ { `CDevice', convertFlags `Set ReadFlags', + } -> `CEvent' #}
foreign import ccall safe "Evdev.chs.h libevdev_next_event"
    libevdevNextEvent :: CDevice -> CUInt -> Ptr CEvent -> IO CInt
nextEvent :: Device -> Set ReadFlags -> IO Event
nextEvent dev flags = do
    ptr <- C2HSImp.mallocForeignPtrBytes {#sizeof input_event #}
    err <- C2HSImp.withForeignPtr ptr (libevdevNextEvent (cDevice dev) (convertFlags flags))
    if err/=0 then
        -- TODO do better by matching err
        ioError (IOError Nothing NoSuchThing "" "couldn't get next event" (Just err)
            (Just $ BS.unpack $ devicePath dev))
    else convertEvent (CEvent ptr)

{#fun libevdev_grab as ^ { `CDevice', convertEnum `GrabMode' } -> `Int' #}
grabDevice :: Device -> IO Int
grabDevice = flip libevdevGrab LibevdevGrab . cDevice
ungrabDevice :: Device -> IO Int
ungrabDevice = flip libevdevGrab LibevdevUngrab . cDevice

-- TODO to we extent can we assume name and filepath don't change?
-- IO exceptions are very possible and should be caught
newDevice :: RawFilePath -> IO Device
newDevice path = do
    Fd n <- openFd path ReadOnly Nothing defaultFileFlags
    dev <- {#call libevdev_new #}
    err <- {#call libevdev_set_fd #} dev n
    if err/=0 then
        -- TODO do better by matching err
        --error $ BS.unpack path ++ " " ++ show err
        ioError (IOError Nothing IllegalOperation "" "couldn't get device" (Just err) (Just $ BS.unpack path))
    else return $ Device dev path

-- seems to depend entirely on whether enableProperty has been set (which always seems to succeed)
-- TODO has_event_type/code may be more useful
{#fun libevdev_has_property as ^ { cDevice `Device', convertEnum `DeviceProperty' } -> `Bool' #}
deviceHasProperty :: DeviceProperty -> Device -> IO Bool
deviceHasProperty = flip libevdevHasProperty
getDeviceProperties :: Device -> IO [DeviceProperty]
getDeviceProperties dev = filterM (flip deviceHasProperty dev) $ enumerate

{#fun libevdev_enable_property as ^ { cDevice `Device', convertEnum `DeviceProperty' } -> `Bool' #}
deviceEnableProperty :: DeviceProperty -> Device -> IO Bool
deviceEnableProperty = fmap not .: flip libevdevEnableProperty

{#fun libevdev_get_name as ^ { cDevice `Device' } -> `String' #}
getDeviceName :: Device -> IO BS.ByteString
getDeviceName = fmap BS.pack . libevdevGetName

-- TODO we want an association between event types and Event data constructors (type families?)
-- otherwise just function Event -> EventType
{#fun libevdev_has_event_type as ^ { cDevice `Device', convertEnum `EventType' } -> `Bool' #}
deviceHasKeyEvents :: Device -> IO Bool
deviceHasKeyEvents dev = libevdevHasEventType dev (EventType {#const EV_KEY #})



-- internal use only

-- newDevice, but also print message if successful
newDeviceP :: RawFilePath -> IO Device
newDeviceP path = do
    dev <- newDevice path
    BS.putStrLn $ "Successfully started listening for events at: " <> path
    return dev

-- TODO shouldnt need this, but I had difficulty installing 'either'
swapEither :: Either a b -> Either b a
swapEither (Left x) = Right x
swapEither (Right x) = Left x
rightToMaybe :: Either a b -> Maybe b
rightToMaybe = \case
    Left _ -> Nothing
    Right x -> Just x

-- retry the action after encountering an exception satisfying p
handleBoolRetry :: Exception e => (e -> Bool) -> Int -> IO a -> IO a
handleBoolRetry p t x = handleBool p (const $ threadDelay t >> handleBoolRetry p t x) x

-- Lists files only, and returns full paths.
lsFiles :: RawFilePath -> IO [RawFilePath]
lsFiles = filterM doesFileExist <=< ((fmap . map . combine) <*> listDirectory)

evdevDir :: RawFilePath
evdevDir = "/dev/input"

--filterM (deviceHasKeyEvents <=< newDevice .  (evdevDir </>) . ("event" <>) . BS.pack . show) [1..20]

-- streaming stuff - move to higher-level API

type EventStream = Stream (Of Event) IO IOException

--TODO a merged stream would also be useful (perhaps more so)
runOnEvents :: (EventStream -> IO ()) -> IO ()
runOnEvents f = do
    iNot <- initINotify
    _ <- addWatch iNot [INotify.Create] evdevDir (void . forkIO . watcher f)
    mapM_ (forkIO . (f . (readEvents <=< (lift . newDeviceP)))) =<< lsFiles evdevDir
    where
        -- Acts only when file (not directory) created.
        watcher :: (EventStream -> IO ()) -> INotify.Event -> IO ()
        watcher f (INotify.Created False path) = do
            BS.putStrLn $ "New device found: " <> (evdevDir </> path)
            handleBoolRetry ((== PermissionDenied) . ioe_type) 100 (f $ readEvents =<< lift (newDeviceP (evdevDir </> path)))
        watcher _ _ = return ()

readEvents :: MonadIO m => Device -> Stream (Of Event) m IOException
readEvents dev =
    let tryIO = try :: IO a -> IO (Either IOException a)
    in  Streaming.untilRight $ liftIO $ fmap swapEither $ tryIO $ nextEvent dev defaultReadFlags
