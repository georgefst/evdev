module Main (main) where

import qualified Data.ByteString.Char8 as BS
import Text.Pretty.Simple (pPrint)

import qualified Streamly.Prelude as S

import Evdev
import Evdev.Stream

main :: IO ()
main = do
    S.mapM_ printDevice allDevices
    BS.putStr "Choose device numbers (separated by whitespace): "
    ns <- BS.words <$> BS.getLine
    let paths = S.fromFoldable $ map ((evdevDir <> "/event") <>) ns
    S.mapM_ pPrint $ readEventsMany $ makeDevices paths

printDevice :: Device -> IO ()
printDevice dev = do
    name <- deviceName dev
    BS.putStrLn $ devicePath dev <> ":\n    " <> name
