module Evdev.Stream (
    allDevicePaths,
    allEvents,
    filteredEvents,
    makeDevices,
    readEvents,
    readEventsMany,
    ) where

import RawFilePath.Directory (doesFileExist,listDirectory)
import System.Posix.ByteString (RawFilePath)
import System.Posix.FilePath ((</>))

import Streamly
import qualified Streamly.Prelude as S

import Evdev


allEvents :: (IsStream t, Monad (t IO)) => t IO (Device, Event)
allEvents = filteredEvents $ const True

filteredEvents :: (IsStream t, Monad (t IO)) => (Device -> Bool) -> t IO (Device, Event)
filteredEvents p = readEventsMany $ S.filter p $ makeDevices allDevicePaths

readEvents :: Device -> SerialT IO Event
readEvents dev = S.repeatM $ nextEvent dev defaultReadFlags

readEventsMany :: IsStream t => AsyncT IO Device -> t IO (Device, Event)
readEventsMany ds = asyncly $ do
    d <- ds
    S.map (d,) $ serially $ readEvents d

makeDevices :: (IsStream t, Functor (t IO)) => t IO RawFilePath -> t IO Device
makeDevices = S.mapMaybeM maybeNewDevice

allDevicePaths :: (IsStream t, Monad (t IO)) => t IO RawFilePath
allDevicePaths = S.filterM doesFileExist $ S.map (evdevDir </>) $ S.fromFoldable =<< S.yieldM (listDirectory evdevDir)
