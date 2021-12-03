module Main (main) where

import qualified Data.ByteString.Char8 as BS
import Text.Pretty.Simple (pPrint)

import qualified Streamly.Prelude as S

import Evdev
import Evdev.Stream

main :: IO ()
main = do
    S.mapM_ printDevice allDevices
    BS.putStr "Choose device numbers (separated by whitespace) (leave blank to read from all, including new ones): "
    ns <- BS.words <$> BS.getLine
    S.mapM_ pPrint $
        readEventsMany
            if null ns
                then allDevices <> newDevices
                else makeDevices $ S.fromFoldable $ map ((evdevDir <> "/event") <>) ns

printDevice :: Device -> IO ()
printDevice dev = do
    name <- deviceName dev
    BS.putStrLn $ devicePath dev <> ":\n    " <> name
