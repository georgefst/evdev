module Evdev.Stream (
    allDevices,
    allEvents,
    makeDevices,
    readEvents,
    readEventsMany,
    ) where

import System.IO
import System.IO.Error

import RawFilePath.Directory (doesFileExist,listDirectory)
import System.Posix.ByteString (RawFilePath)
import System.Posix.FilePath ((</>))

import Streamly
import qualified Streamly.Prelude as S

import Evdev


-- | Read all events from a device.
readEvents :: Device -> SerialT IO Event
readEvents dev = S.repeatM $ nextEvent dev defaultReadFlags

-- | Concurrently read events from multiple devices.
readEventsMany :: IsStream t => AsyncT IO Device -> t IO (Device, Event)
readEventsMany ds = asyncly $ do
    d <- ds
    S.map (d,) $ serially $ readEvents d

-- | Create devices for all paths in the stream.
-- | Will throw an exception if a path doesn't correspond to a valid input device.
makeDevices :: IsStream t => t IO RawFilePath -> t IO Device
makeDevices = S.mapM newDevice

-- | All events on all valid devices (in /dev/input).
allEvents :: IsStream t => t IO (Device, Event)
allEvents = readEventsMany allDevices

-- | All valid devices (in /dev/input).
allDevices :: (IsStream t, Monad (t IO)) => t IO Device
allDevices =
    let maybeNewDevice path = (Just <$> newDevice path) `catchIOError` \err -> do
            hPrint stderr err --TODO use stderr
            return Nothing
        paths = S.filterM doesFileExist $ S.map (evdevDir </>) $ S.fromFoldable =<< S.yieldM (listDirectory evdevDir)
    in  S.mapMaybeM maybeNewDevice paths
