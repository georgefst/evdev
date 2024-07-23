module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Data.Either.Extra
import Data.Foldable.Extra
import Data.Functor
import Data.Maybe
import Data.Time
import Evdev
import Evdev.Codes
import qualified Evdev.Uinput as Uinput
import qualified System.Directory.OsPath
import System.IO.Error
import System.OsPath.Posix
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import System.OsString.Internal.Types (OsString(..))

main :: IO ()
main = defaultMain $ testGroup "Tests" [smoke, inverses]

{- | Just checks that we can create a virtual device, find it, see that it has the expected properties,
and read the expected events.
-}
smoke :: TestTree
smoke = testCase "Smoke" do
    start <- newEmptyMVar
    let duName = "evdev-test-device"
        keys = [Key1 .. Key0]
        evs = concatMap ((<$> [Pressed, Released]) . KeyEvent) keys
    du <- Uinput.newDevice duName Uinput.defaultDeviceOpts{Uinput.keys}
    void $ forkIO do
        takeMVar start -- wait until reading device is initialised
        Uinput.writeBatch du evs
    listDirectory evdevDir
        >>= traverse (fmap eitherToMaybe . try @IOError . (retryIf isPermissionError . newDevice) . (evdevDir </>))
        >>= findM (fmap (== duName) . deviceName) . catMaybes
        >>= \case
            Nothing -> assertFailure "Couldn't find device with correct name"
            Just d -> do
                putMVar start ()
                (@?= Nothing) =<< devicePhys d
                (@?= Nothing) =<< deviceUniq d
                (@?= [EvSyn, EvKey]) =<< deviceEventTypes d
                evs' <- whileJust ((\x -> guard (x /= last evs) $> x) . eventData <$> nextEvent d) pure
                filter (/= SyncEvent SynReport) evs' @?= init evs

inverses :: TestTree
inverses =
    localOption (QuickCheckTests 1000) . testGroup "Inverses" $
        [ testGroup
            "TimeVal"
            [ testProperty "1" \(s, us) ->
                let tv = CTimeVal s us
                 in s < 0 || us < 0 || us >= 1_000_000 || toCTimeVal (fromCTimeVal tv) == tv
            , testProperty "2" \n ->
                let -- 'toCTimeVal' goes from picoseconds to microseconds
                    resolutionFactor = 1_000_000
                 in abs (diffTimeToPicoseconds (fromCTimeVal . toCTimeVal $ picosecondsToDiffTime n) - n)
                        < resolutionFactor
            ]
        , testProperty "EventData" \x@(t, c, _v) ->
            let x'@(t', c', v') = toCEventData (fromCEventData x)
                syncValueZero =
                    -- 'toCEventData' takes all values for sync events to 0 - fine as they don't mean anything
                    and
                        [ t == t'
                        , fromEnum t == fromEnum EvSyn
                        , c == c'
                        , v' == 0
                        ]
             in x' == x || syncValueZero
        ]

--TODO make delay and max retries configurable, add to library?
retryIf :: forall a e. Exception e => (e -> Bool) -> IO a -> IO a
retryIf p x = go 100
  where
    go :: Word -> IO a
    go tries =
        x `catch` \e ->
            if p e && tries /= 0 then threadDelay 10_000 >> go (tries - 1) else throw e

-- TODO copied from `evdev-streamly` - see there for issues
listDirectory :: PosixPath -> IO [PosixPath]
listDirectory = fmap (map getOsString) . System.Directory.OsPath.listDirectory . OsString
