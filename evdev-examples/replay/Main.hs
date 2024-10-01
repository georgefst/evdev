module Main (main) where

import Control.Concurrent
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Time
import System.Environment
import System.OsPath.Posix
import Text.Read

import Streamly.Prelude qualified as S

import Evdev
import Evdev.Stream
import Evdev.Uinput hiding (Device, newDevice)
import Evdev.Uinput qualified as Uinput

main :: IO ()
main = getArgs >>= \case
    "record" : (encodeUtf -> Just dev) : ((\case ["grab"] -> Just True; [] -> Just False; _ -> Nothing) -> Just grab) -> do
        d <- newDevice dev
        when grab $ grabDevice d
        S.mapM_ print $ readEvents d
    ["replay"] -> do
        evs <- map (fromMaybe (error "failed to parse event") . readMaybe) . lines <$> getContents
        d <- Uinput.newDevice "evdev-replay" . deviceOptsFromEvents Nothing Nothing $ map eventData evs
        traverse_
            (\(e, t) -> threadDelay (fromInteger $ diffTimeToPicoseconds t `div` 1_000_000) >> writeEvent d e)
            ( maybe id ((:) . (\e -> (e.eventData, 0))) (listToMaybe evs) $
                zipWith (\e1 e2 -> (e2.eventData, e2.eventTime - e1.eventTime)) evs (tail evs)
            )
    _ -> error "bad args - try \"evdev-replay record /dev/input/event2 grab\" or \"evdev-replay replay\""
