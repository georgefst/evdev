module Main (main) where

import Control.Concurrent
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.String
import Data.Time
import System.Environment
import Text.Read

import Control.Monad.State (execState, modify)
import Streamly.Prelude qualified as S

import Evdev
import Evdev.Stream
import Evdev.Uinput hiding (Device, newDevice)
import Evdev.Uinput qualified as Uinput

main :: IO ()
main = getArgs >>= \case
    "record" : dev : ((\case ["grab"] -> Just True; [] -> Just False; _ -> Nothing) -> Just grab) -> do
        d <- newDevice $ fromString dev
        when grab $ grabDevice d
        S.mapM_ print $ readEvents d
    ["replay"] -> do
        evs <- map (fromMaybe (error "failed to parse event") . readMaybe) . lines <$> getContents
        d <- Uinput.newDevice "evdev-replay" $ flip execState defaultDeviceOpts $ for_ (map eventData evs) \case
            SyncEvent _ -> pure ()
            KeyEvent e _ -> modify \o -> o{keys = e : o.keys}
            RelativeEvent e _ -> modify \o -> o{relAxes = e : o.relAxes}
            AbsoluteEvent e _ -> modify \o -> o{absAxes = (e, AbsInfo 0 0 0 0 0 0) : o.absAxes}
            MiscEvent e _ -> modify \o -> o{miscs = e : o.miscs}
            SwitchEvent e _ -> modify \o -> o{switchs = e : o.switchs}
            LEDEvent e _ -> modify \o -> o{leds = e : o.leds}
            SoundEvent e _ -> modify \o -> o{sounds = e : o.sounds}
            RepeatEvent e _ -> modify \o -> o{reps = (e, 0) : o.reps}
            ForceFeedbackEvent e _ -> modify \o -> o{ffs = e : o.ffs}
            PowerEvent e _ -> modify \o -> o{powers = e : o.powers}
            ForceFeedbackStatusEvent e _ -> modify \o -> o{ffStats = e : o.ffStats}
            UnknownEvent{} -> pure ()
        traverse_
            (\(e, t) -> threadDelay (fromInteger $ diffTimeToPicoseconds t `div` 1_000_000) >> writeEvent d e)
            ( maybe id ((:) . (\e -> (e.eventData, 0))) (listToMaybe evs) $
                zipWith (\e1 e2 -> (e2.eventData, e2.eventTime - e1.eventTime)) evs (tail evs)
            )
    _ -> error "bad args - try \"evdev-replay record /dev/input/event2 grab\" or \"evdev-replay replay\""
