module Main where

import qualified Data.ByteString.Char8 as BS

import qualified Streamly.Prelude as S
import Streamly

import Evdev
import Evdev.Stream

main :: IO ()
main = do
    forM' allDevices $ \dev -> do
        let path = devicePath dev
        name <- deviceName dev
        BS.putStrLn $ path <> ":" <> BS.replicate (24 - BS.length path) ' ' <> name
    BS.putStr "Choose device numbers (separated by whitespace): "
    ns <- BS.words <$> BS.getLine
    let paths = S.fromFoldable $ map ((evdevDir <> "/event") <>) ns
    forM' (readEventsMany $ makeDevices paths) $ \(d,e) ->
        BS.putStrLn $ devicePath d <> ": " <> BS.pack (prettyEvent e)

forM' :: Monad m => SerialT m a -> (a -> m b) -> m ()
forM' = flip S.mapM_
