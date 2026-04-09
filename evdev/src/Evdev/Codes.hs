{-# LANGUAGE TemplateHaskell #-}

{- | Datatypes corresponding to the constants in [input-event-codes.h](https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h).
See [the Linux Kernel documentation](https://www.kernel.org/doc/html/latest/input/event-codes.html) for full details, noting that all names have been mechanically transformed into CamelCase.
-}
module Evdev.Codes where

import Data.Char
import Data.List
import Data.Maybe
import Data.Tuple.Extra
import Evdev.Codes.Generator
import Evdev.Raw
import Language.Haskell.TH
import System.Process
import Util

$( do
    libc <-
        dropWhile isSpace
            . fromMaybe (error "bad cpp response")
            . find ("libc" `isInfixOf`)
            . dropWhile (not . ("#include" `isPrefixOf`))
            . lines
            . thd3
            <$> runIO (readProcessWithExitCode "cpp" ["-v"] "")
    generateCodes $ libc <> "/linux/input-event-codes.h"
 )
