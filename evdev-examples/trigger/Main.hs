module Main (main) where

import System.Environment (getArgs)
import System.Process (readProcess)

import qualified Streamly.Prelude as S

import Evdev
import Evdev.Stream

-- | Run a command on any occurrence of the given key, across all devices.
main :: IO ()
main = getArgs >>= \case
    (read -> !key) : p : ps ->
        flip S.mapM_ (snd <$> readEventsMany allDevices) \case
            Event{eventData = KeyEvent k Pressed} | k == key ->
                putStrLn =<< readProcess p ps ""
            _ -> pure ()
    _ -> error "bad args - try \"trigger Key1 wmctrl -s 1\""
