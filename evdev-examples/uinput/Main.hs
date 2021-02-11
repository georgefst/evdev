module Main (main) where

import Control.Monad

import Evdev
import qualified Evdev.Codes as Codes
import Evdev.Uinput

main :: IO ()
main = do
    dev <- newUDevice (defaultNewUDevice "haskell-test"){keys = keys'}
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
