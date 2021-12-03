module Main (main) where

import qualified Data.ByteString.Char8 as BS
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)

import qualified Streamly.Prelude as S

import Evdev
import Evdev.Stream

--TODO display usage example in CLI
main :: IO ()
main = do
    devs <- getArgs >>= \case
        ["all"] -> pure allDevices
        ["new", t] -> pure $ S.mapM (\d -> printNewDevice d >> pure d) $ newDevices' $ read t
          where
            printNewDevice d = do
                BS.putStrLn ""
                BS.putStrLn "New device:"
                BS.putStrLn $ devicePath d
                BS.putStrLn =<< deviceName d
                BS.putStrLn ""
        _ -> do
            S.mapM_ printDevice allDevices
            BS.putStr "Choose device numbers (separated by whitespace): "
            ns <- BS.words <$> BS.getLine
            pure $ makeDevices $ S.fromFoldable $ map ((evdevDir <> "/event") <>) ns
    S.mapM_ pPrint $ readEventsMany devs

printDevice :: Device -> IO ()
printDevice dev = do
    name <- deviceName dev
    BS.putStrLn $ devicePath dev <> ":\n    " <> name
