module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Data.Either.Extra
import Data.Foldable.Extra
import Data.Functor
import Data.Maybe
import Evdev
import Evdev.Codes
import RawFilePath
import System.FilePath.ByteString
import System.IO.Error
import Test.Tasty
import Test.Tasty.HUnit

{-TODO
doesn't work relably in GHCI
    if we fail to get a device in any run after the first, we just hang for some reason
-}

{- | Currently just checks that we can create a virtual device, find it, see that it has the expected properties,
and read the expected events.
-}
main :: IO ()
main = do
    start <- newEmptyMVar
    let duName = "evdev-test-device"
        keys = [Key1 .. Key0]
        evs = concatMap ((<$> [Pressed, Released]) . KeyEvent) keys
    du <- newUDevice (defaultNewUDevice duName){keys}
    void $ forkIO do
        takeMVar start -- wait until reading device is initialised
        writeBatch du evs
    defaultMain . testCase "Test 1" $
        listDirectory evdevDir
            >>= traverse (fmap eitherToMaybe . try @IOError . (retryIf isPermissionError . newDevice) . (evdevDir </>))
            >>= findM (fmap (== duName) . deviceName) . catMaybes
            >>= \case
                Nothing -> assertFailure "Couldn't find device with correct name"
                Just d -> do
                    putMVar start ()
                    (@?= [EvSyn, EvKey]) =<< deviceEventTypes d
                    evs' <- whileJust ((\x -> guard (x /= last evs) $> x) . eventData <$> nextEvent d) pure
                    filter (/= SyncEvent SynReport) evs' @?= init evs

--TODO make delay and max retries configurable, add to library?
retryIf :: forall a e. Exception e => (e -> Bool) -> IO a -> IO a
retryIf p x = go 100
  where
    go :: Word -> IO a
    go tries =
        x `catch` \e ->
            if p e && tries /= 0 then threadDelay 10_000 >> go (tries - 1) else throw e
