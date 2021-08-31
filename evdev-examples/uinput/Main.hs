module Main (main) where

import Control.Monad

import qualified Evdev.Codes as Codes
import Evdev.Uinput

main :: IO ()
main = do
    dev <- newDevice "haskell-test" defaultDeviceOpts{keys = keys'}
    forever $ do
        _ <- getChar
        writeBatch dev events

events :: [EventData]
events = concat [[KeyEvent k a | a <- [Pressed, Released]] | k <- keys']

keys' :: [Codes.Key]
keys' =
    [ Codes.KeyLeftctrl
    , Codes.KeyLeftshift
    ]
