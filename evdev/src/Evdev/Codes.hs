{-# LANGUAGE TemplateHaskell #-}

{- | Datatypes corresponding to the constants in [input-event-codes.h](https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h).
See [the Linux Kernel documentation](https://www.kernel.org/doc/html/latest/input/event-codes.html) for full details, noting that all names have been mechanically transformed into CamelCase.
-}
module Evdev.Codes where

import Control.Monad
import Evdev.Codes.Generator
import Evdev.Raw
import Language.Haskell.TH
import System.Directory
import System.Environment
import System.FilePath
import Util

$( do
    candidates <-
        runIO $
            map (<> "/linux/input-event-codes.h")
                . (<> ["/usr/include"])
                . maybe [] splitSearchPath
                <$> lookupEnv "C_INCLUDE_PATH"
    runIO (filterM doesFileExist candidates) >>= \case
        d : _ -> generateCodes d
        [] -> error $ "Could not find input-event-codes.h. Install Linux headers or try setting C_INCLUDE_PATH."
 )
