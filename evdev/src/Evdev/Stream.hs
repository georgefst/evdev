module Evdev.Stream (
    allDevices,
    allEvents,
    makeDevices,
    readEvents,
    readEventsMany,
    ) where

import Control.Exception
import Control.Monad
import System.IO.Error

import RawFilePath.Directory (doesFileExist,listDirectory)
import System.Posix.ByteString (RawFilePath)
import System.Posix.FilePath ((</>))

import Streamly
import qualified Streamly.Prelude as S

import Evdev


-- read all events from a device
readEvents :: Device -> SerialT IO Event
readEvents dev = S.repeatM $ nextEvent dev defaultReadFlags

-- concurrently read events from multiple devices
readEventsMany :: IsStream t => AsyncT IO Device -> t IO (Device, Event)
readEventsMany ds = asyncly $ do
    d <- ds
    S.map (d,) $ serially $ readEvents d

-- will throw an exception if a path doesn't correspond to a valid input device
makeDevices :: IsStream t => t IO RawFilePath -> t IO Device
makeDevices = S.mapM newDevice

-- all events on all valid devices
allEvents :: IsStream t => t IO (Device, Event)
allEvents = readEventsMany allDevices

-- all paths in evdevDir corresponding to valid input devices
allDevices :: (IsStream t, Monad (t IO)) => t IO Device
allDevices =
    let maybeNewDevice path = catchJust (guard . isIllegalOperation) (Just <$> newDevice path) (\() -> return Nothing)
        paths = S.filterM doesFileExist $ S.map (evdevDir </>) $ S.fromFoldable =<< S.yieldM (listDirectory evdevDir)
    in  S.mapMaybeM maybeNewDevice paths
