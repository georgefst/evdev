module Main (main) where

import Control.Monad

import Evdev
import qualified Evdev.Codes as Codes

main :: IO ()
main = do
    dev <- newUDevice "haskell-test"
    forever $ do
       _ <- getChar
       writeBatch dev events

events :: [EventData]
events = concat
    [ [ KeyEvent k a | a <- [Pressed, Released] ]
    | k <-
        [ Codes.KeyLeftmeta
        , Codes.KeyA
        , Codes.KeyB
        ]
    ]
