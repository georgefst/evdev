module Main (main) where

import Control.Exception
import Control.Monad
import Data.Maybe
import System.Exit
import System.IO.Error

import qualified Evdev.Codes as Codes
import Evdev.Uinput

main :: IO ()
main = do
    dev <-
        newDevice
            "haskell-uinput-echo-example"
            defaultDeviceOpts{keys = mapMaybe charToEvent (['a' .. 'z'] ++ ['0' .. '9'])}
    forever do
        cs <-
            catchJust
                (guard . isEOFError)
                getLine
                \() -> exitSuccess
        writeBatch dev [KeyEvent k a | Just k <- map charToEvent cs, a <- [Pressed, Released]]

charToEvent :: Char -> Maybe Codes.Key
charToEvent = \case
    '0' -> Just Codes.Key0
    '1' -> Just Codes.Key1
    '2' -> Just Codes.Key2
    '3' -> Just Codes.Key3
    '4' -> Just Codes.Key4
    '5' -> Just Codes.Key5
    '6' -> Just Codes.Key6
    '7' -> Just Codes.Key7
    '8' -> Just Codes.Key8
    '9' -> Just Codes.Key9
    'a' -> Just Codes.KeyA
    'b' -> Just Codes.KeyB
    'c' -> Just Codes.KeyC
    'd' -> Just Codes.KeyD
    'e' -> Just Codes.KeyE
    'f' -> Just Codes.KeyF
    'g' -> Just Codes.KeyG
    'h' -> Just Codes.KeyH
    'i' -> Just Codes.KeyI
    'j' -> Just Codes.KeyJ
    'k' -> Just Codes.KeyK
    'l' -> Just Codes.KeyL
    'm' -> Just Codes.KeyM
    'n' -> Just Codes.KeyN
    'o' -> Just Codes.KeyO
    'p' -> Just Codes.KeyP
    'q' -> Just Codes.KeyQ
    'r' -> Just Codes.KeyR
    's' -> Just Codes.KeyS
    't' -> Just Codes.KeyT
    'u' -> Just Codes.KeyU
    'v' -> Just Codes.KeyV
    'w' -> Just Codes.KeyW
    'x' -> Just Codes.KeyX
    'y' -> Just Codes.KeyY
    'z' -> Just Codes.KeyZ
    _ -> Nothing
