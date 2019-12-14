module Evdev.Stream (
    allDevicePaths,
    allEvents,
    filteredEvents,
    makeDevices,
    readEvents,
    readEventsMany,
    ) where

import Control.Monad

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

readEvents :: IsStream t => Device -> t IO Event
readEvents dev = S.repeatM $ nextEvent dev defaultReadFlags

readEventsMany :: IsStream t => AsyncT IO Device -> t IO (Device, Event)
readEventsMany ds = asyncly $ do
    d <- ds
    S.map (d,) $ readEvents d

makeDevices :: (IsStream t, Functor (t IO)) => t IO RawFilePath -> t IO Device
makeDevices = S.mapMaybeM maybeNewDevice

--TODO use Streamly directly
allDevicePaths :: (IsStream t, Monad (t IO)) => t IO RawFilePath
allDevicePaths = do
    fs <- S.yieldM $ lsFiles evdevDir
    S.fromFoldable fs


{- Util -}

-- lists files only, and returns full paths.
lsFiles :: RawFilePath -> IO [RawFilePath]
lsFiles p = filterM doesFileExist =<< (map (p </>) <$> listDirectory p)
