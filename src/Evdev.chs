module Evdev (
      Device
    , Event (..)
    , EventStream
    , Fd (..)
    , Key
    , KeyEventType (..)
    , defaultReadFlags
    , devicePath
    , grabDevice
    , newDevice
    , nextEvent
    , runOnEvents
    , ungrabDevice
) where

import Control.Concurrent
import Control.Exception.Extra
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import qualified Data.ByteString.Char8 as BS
import Data.Set (Set)
import Foreign (Ptr,(.|.))
import Foreign.C (CInt,CUInt)
import GHC.IO.Exception (IOException(IOError),IOErrorType(..),ioe_type) -- not sure we shoudl use this...
import RawFilePath.Directory (doesFileExist,listDirectory)
import Streaming.Prelude (Of,Stream)
import qualified Streaming.Prelude as Streaming
import System.INotify (addWatch,initINotify)
import qualified System.INotify as INotify
import System.Posix.ByteString (RawFilePath)
import System.Posix.FilePath ((</>),combine)
import System.Posix.IO.ByteString (OpenMode(ReadOnly),defaultFileFlags,openFd)
import System.Posix.Types (Fd(Fd))

import Evdev.Codes (Key)

#include <libevdev-1.0/libevdev/libevdev.h>
#include <linux/input.h>

-- unfortunately the prefix part of the hook seems not to work
{#enum libevdev_read_flag as ReadFlags { LIBEVDEV_READ_FLAG_SYNC as Sync, LIBEVDEV_READ_FLAG_NORMAL as Normal, LIBEVDEV_READ_FLAG_FORCE_SYNC as ForceSync, LIBEVDEV_READ_FLAG_BLOCKING as Blocking } deriving (Eq,Ord,Show) #}
-- blocking seems to be necessary to stop us from only getting half the events
defaultReadFlags :: Set ReadFlags
defaultReadFlags = [Normal,Blocking]

{#pointer *input_event as CEvent foreign newtype#}
-- doesnt seem there's a way to pass this to fun (more complicated than others - output, pointer, IO, +)
convertEvent :: CEvent -> IO Event
convertEvent ev = do
    let getField f = withCEvent ev (fmap fromIntegral . f)
    classifyEvent
        <$> getField {#get input_event->type #}
        <*> getField {#get input_event->code #}
        <*> getField {#get input_event->value #}

classifyEvent :: Int -> Int -> Int -> Event
classifyEvent t c v -- type, code, value
    | t == {#const EV_KEY #} = KeyEvent {evKeyCode = toEnum c, evKeyEventType = toEnum v}
    | otherwise              = MscEvent {evType = t, evCode = c, evValue = v}

-- unofrtunately it seems impossible to use '+' without discarding the original output data
-- so we have to do this bit manually
-- TODO PR on c2hs to add this functionality
--{#fun libevdev_next_event as ^ { `CDevice', convertFlags `Set ReadFlags', + } -> `CEvent' #}
foreign import ccall safe "Evdev.chs.h libevdev_next_event"
    libevdevNextEvent :: CDevice -> CUInt -> Ptr CEvent -> IO CInt
nextEvent :: Device -> Set ReadFlags -> IO Event
nextEvent dev flags = do
    ptr <- C2HSImp.mallocForeignPtrBytes {#sizeof input_event #}
    err <- C2HSImp.withForeignPtr ptr (libevdevNextEvent (cDevice dev) (convertFlags flags))
    if err/=0 then
        ioError (IOError Nothing NoSuchThing "" "couldn't get next event" (Just err)
            (Just $ BS.unpack $ devicePath dev))
    else convertEvent (CEvent ptr)

{#pointer *libevdev as CDevice newtype #}
convertFlags :: Set ReadFlags -> CUInt
convertFlags = fromIntegral . foldr ((.|.).fromEnum) 0

enumToCInt :: Enum a => a -> CInt
enumToCInt = fromIntegral . fromEnum
{#fun libevdev_grab as ^ { `CDevice', enumToCInt `GrabMode' } -> `Int' #}
{#enum libevdev_grab_mode as GrabMode { underscoreToCase } deriving (Show) #}
grabDevice :: Device -> IO Int
grabDevice = flip libevdevGrab LibevdevGrab . cDevice
ungrabDevice :: Device -> IO Int
ungrabDevice = flip libevdevGrab LibevdevUngrab . cDevice

newDevice :: RawFilePath -> IO Device
newDevice path = do
    Fd n <- openFd path ReadOnly Nothing defaultFileFlags
    dev <- {#call libevdev_new #}
    err <- {#call libevdev_set_fd #} dev n
    if err/=0 then
        ioError (IOError Nothing IllegalOperation "" "couldn't get device" (Just err) (Just $ BS.unpack path))
    else return $ Device dev path

-- TODO add more event types
-- should have time field
data Event = SyncEvent { evSyncCode :: SyncType }
           | KeyEvent  { evKeyCode :: Key
                       , evKeyEventType :: KeyEventType }
           | MscEvent  { evType :: Int -- TODO use enum
                       , evCode :: Int
                       , evValue :: Int }
           deriving (Show, Eq)
data Device = Device { cDevice :: CDevice, devicePath :: RawFilePath }
newtype SyncType = SyncType Int deriving (Show, Eq)
data KeyEventType =  Released | Pressed | Repeated
    deriving (Show, Eq, Ord, Enum)


type EventStream = Stream (Of Event) IO IOException

runOnEvents :: Bool -> (EventStream -> IO ()) -> IO ()
runOnEvents wait f = do
    iNot <- initINotify
    _ <- addWatch iNot [INotify.Create] evDir (void . forkIO . watcher f)
    mapM_ (forkIO . (f . (readEvents <=< (lift . newDeviceP)))) =<< lsFiles evDir
    when wait $ forever $ threadDelay maxBound -- apparently people do this

watcher :: (EventStream -> IO ()) -> INotify.Event -> IO ()
watcher f (INotify.Created False path) = do -- file (not directory) created
    BS.putStrLn $ "New device found: " <> (evDir </> path) -- TODO shouldn't print - allow callback?
    handleBoolRetry ((== PermissionDenied) . ioe_type) 100 (f $ readEvents =<< lift (newDeviceP (evDir </> path)))
watcher _ _ = return ()

readEvents :: MonadIO m => Device -> Stream (Of Event) m IOException
readEvents dev =
    let tryIO = try :: IO a -> IO (Either IOException a)
    in  Streaming.untilRight $ liftIO $ fmap swapEither $ tryIO $ nextEvent dev defaultReadFlags

-- retry the action after encountering an exception satisfying p
handleBoolRetry :: Exception e => (e -> Bool) -> Int -> IO a -> IO a
handleBoolRetry p t x = handleBool p (const $ threadDelay t >> handleBoolRetry p t x) x

-- returns full paths
-- this could be clearer but hey I get to use <*> as the S combinator
lsFiles :: RawFilePath -> IO [RawFilePath]
lsFiles = filterM doesFileExist <=< ((fmap . map . combine) <*> listDirectory)

evDir :: RawFilePath
evDir = "/dev/input"

-- newDevice, but also print message if successful
-- IOExceptions are very possible and should be caught
newDeviceP :: RawFilePath -> IO Device
newDeviceP path = do
    dev <- newDevice path
    BS.putStrLn $ "Successfully started listening for events at: " <> path
    return dev

-- TODO shouldnt need this, but I had difficulty installing 'either'
swapEither :: Either a b -> Either b a
swapEither (Left x) = Right x
swapEither (Right x) = Left x
