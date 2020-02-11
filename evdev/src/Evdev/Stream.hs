-- | The high-level, stream-based API.
-- Unless stated otherwise, these functions will throw exceptions if the underlying C calls fail.
module Evdev.Stream (
    allDevices,
    allEvents,
    makeDevices,
    newDevices,
    readEvents,
    readEventsMany,
) where

import Data.Bool
import Data.Either.Extra
import Data.Functor
import System.IO
import System.IO.Error

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BS
import RawFilePath.Directory (doesFileExist,listDirectory)
import qualified Streamly.FSNotify as N
import Streamly.FSNotify (EventPredicate(EventPredicate),FSEntryType(NotDir),watchDirectory)
import System.Path (Absolute,Path,fromFilePath,toFilePath)
import System.Posix.ByteString (RawFilePath)
import System.Posix.FilePath ((</>))

import Streamly
import qualified Streamly.Prelude as S

import Evdev


-- | Read all events from a device.
readEvents :: Device -> SerialT IO Event
readEvents dev = S.repeatM $ nextEvent dev defaultReadFlags

-- | Concurrently read events from multiple devices.
-- If a read fails on one, the exception is printed to stderr and the stream continues to read from the others.
readEventsMany :: IsStream t => AsyncT IO Device -> t IO (Device, Event)
readEventsMany ds = asyncly $ do
    d <- ds
    S.map (d,) $ serially $ readEvents' d
    where
        -- catch all IO errors
        readEvents' :: Device -> SerialT IO Event
        readEvents' dev = unfoldM $ printIOError' $ nextEvent dev defaultReadFlags

-- | Create devices for all paths in the stream.
makeDevices :: IsStream t => t IO RawFilePath -> t IO Device
makeDevices = S.mapM newDevice

-- | All events on all valid devices (in /\/dev\/input/).
-- Prints any exceptions.
--
-- > allEvents == readEventsMany allDevices
allEvents :: IsStream t => t IO (Device, Event)
allEvents = readEventsMany allDevices

--TODO call this 'oldDevices' or 'existingDevices', and have 'allDevices' include 'newDevices'?
-- | All valid existing devices (in /\/dev\/input/).
-- If a device can't be initialised for an individual path, then the exception is printed,
-- and the function continues to try to initialise the others.
allDevices :: (IsStream t, Monad (t IO)) => t IO Device
allDevices =
    let paths = S.filterM doesFileExist $ S.map (evdevDir </>) $ S.fromFoldable =<< S.yieldM (listDirectory evdevDir)
    in  S.mapMaybeM (printIOError' . newDevice) paths

-- | All new devices created (in /\/dev\/input/).
-- Watches for new file paths (using \inotify\), and those corresponding to valid devices are added to the stream.
newDevices :: (IsStream t, Monad (t IO)) => t IO Device
newDevices =
    let -- 'watching' keeps track of the set of paths which have been added, but don't yet have the right permissions
        watch :: Set (Path Absolute) -> N.Event -> IO (Maybe Device, Set (Path Absolute))
        watch watching = \case
            N.Added p _ NotDir ->
                tryNewDevice p <&> \case
                    Right d -> -- success - return new device
                        (Just d, watching)
                    Left e -> -- fail - if it's only a permission error then watch for changes on device
                        (Nothing, applyWhen (isPermissionError e) (Set.insert p) watching)
            N.Modified p _ NotDir ->
                if p `elem` watching then
                    tryNewDevice p <&> \case
                        Right d -> -- success - no longer watch for changes
                            (Just d, Set.delete p watching)
                        Left _ -> -- fail - continue to watch
                            (Nothing, watching)
                else -- this isn't an event we care about
                    return (Nothing, watching)
            N.Removed p _ NotDir -> -- device is gone - no longer watch for changes
                return (Nothing, Set.delete p watching)
            _ -> return (Nothing, watching)
        tryNewDevice = printIOError . newDevice . BS.pack . toFilePath
    in do
        (_,es) <- S.yieldM $ watchDirectory (fromFilePath $ BS.unpack evdevDir) (EventPredicate $ const True)
        scanMaybe watch [] es


{- Util -}

-- specialized form of S.scanlM'
-- for each a, f updates s, and possibly produces a new b, to add to the output stream
-- I really can't think of a good name for this...
-- TODO perhaps some way to use State monad instead?
scanMaybe :: (IsStream t, Monad m) => (s -> a -> m (Maybe b, s)) -> s -> t m a -> t m b
scanMaybe f e  = S.mapMaybe fst . S.scanlM' (f . snd) (Nothing, e)

-- specialised form of S.unfoldrM
-- this should perhaps be in streamly (it's in monad-loops)
--TODO this is rather ugly - can it be done in terms of the Unfold type?
unfoldM :: (IsStream t, MonadAsync m) => m (Maybe a) -> t m a
unfoldM x = S.unfoldrM (const $ fmap (,undefined) <$> x) undefined

-- like tryIOError, but also prints the error to stderr
printIOError :: IO a -> IO (Either IOError a)
printIOError f = (Right <$> f) `catchIOError` \err -> do
    hPrint stderr err
    return $ Left err

-- variant of printIOError which doesn't care what the exception was
printIOError' :: IO a -> IO (Maybe a)
printIOError' = fmap eitherToMaybe . printIOError

-- apply the function iff the guard passes
applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen = flip $ bool id
