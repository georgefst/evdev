module Evdev.Stream where

import Control.Concurrent
import Control.Exception.Extra
import Control.Monad
import Control.Monad.IO.Class
import Data.Either.Combinators

import GHC.IO.Exception (IOErrorType(PermissionDenied),ioe_type)
import RawFilePath.Directory (doesFileExist,listDirectory)
import System.INotify (addWatch,initINotify)
import qualified System.INotify as I
import System.Posix.ByteString (RawFilePath)
import System.Posix.FilePath ((</>),combine)

import Streamly
import qualified Streamly.Prelude as S

import Evdev

allEvents :: IsStream t => t IO (Device, Event)
allEvents = filteredEvents $ const True

filteredEvents :: IsStream t => (Device -> Bool) -> t IO (Device, Event)
filteredEvents p = readEventsMany $ S.filter p $ makeDevices $ existingDevicePaths <> newDevicePaths

-- reads until encountering an IOException
readEvents :: IsStream t => Device -> t IO Event
readEvents dev = serially $ unfoldrM' $ fmap rightToMaybe $ tryIO $ nextEvent dev defaultReadFlags

readEventsMany :: IsStream t => AsyncT IO Device -> t IO (Device, Event)
readEventsMany = asyncly . join . (S.map $ \d -> (S.map (d,) $ readEvents d))

makeDevices :: (Functor (t IO), IsStream t) => t IO RawFilePath -> t IO Device
makeDevices = S.mapMaybeM maybeNewDevice

existingDevicePaths :: (IsStream t, MonadIO (t IO)) => t IO RawFilePath
existingDevicePaths = S.fromFoldable =<< S.yieldM (lsFiles evdevDir)

newDevicePaths :: (MonadIO (t IO), IsStream t) => t IO RawFilePath
newDevicePaths =
    let watcher mvar = \case
            I.Created False path -> do -- file (not directory) created
                let fullPath = evdevDir </> path
                handleBoolRetry ((== PermissionDenied) . ioe_type) 100 $ putMVar mvar fullPath
            _ -> return ()
    in do
        mvar <- liftIO newEmptyMVar
        iNot <- liftIO initINotify
        _ <- liftIO $ addWatch iNot [I.Create] evdevDir (watcher mvar)
        serially $ S.repeatM $ takeMVar mvar


-- retry the action after encountering an exception satisfying p
handleBoolRetry :: Exception e => (e -> Bool) -> Int -> IO a -> IO a
handleBoolRetry p t x = handleBool p (const $ threadDelay t >> handleBoolRetry p t x) x


{- Util -}

-- a specialization of S.unfoldrM which doesn't make use of any value from the previous round
unfoldrM' :: (IsStream t, MonadAsync m) => m (Maybe a) -> t m a
unfoldrM' x = S.unfoldrM (const $ fmap (,undefined) <$> x) undefined

-- lists files only, and returns full paths.
lsFiles :: RawFilePath -> IO [RawFilePath]
lsFiles = filterM doesFileExist <=< ((fmap . map . combine) <*> listDirectory)
