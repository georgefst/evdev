{-# LANGUAGE TemplateHaskell #-}

{- | Datatypes corresponding to the constants in [input-event-codes.h](https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h).
See [the Linux Kernel documentation](https://www.kernel.org/doc/html/latest/input/event-codes.html) for full details, noting that all names have been mechanically transformed into CamelCase.
-}
module Evdev.Codes where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Evdev.Codes.Generator
import Evdev.Raw
import Language.Haskell.TH
import System.Directory
import System.Environment
import Util

$( do
    -- Find linux/input-event-codes.h. On Nix, we search the include dirs from
    -- BINDGEN_EXTRA_CLANG_ARGS. On non-Nix, it's in /usr/include.
    let header = "linux/input-event-codes.h"
        -- Parse -I, -isystem, and -idirafter flags from clang args.
        -- These can appear as "-isystem/path" or "-isystem /path" (two tokens).
        includeDirs = go . words
          where
            twoPartFlags = ["-isystem", "-idirafter", "-iprefix", "-iwithprefix"]
            go [] = []
            go (flag : dir : rest) | flag `elem` twoPartFlags = dir : go rest
            go (w : rest)
                | Just dir <- stripPrefix "-isystem" w <|> stripPrefix "-idirafter" w <|> stripPrefix "-I" w = dir : go rest
                | otherwise = go rest
        findHeader dirs = runIO $ listToMaybe <$> filterM (\d -> doesFileExist $ d <> "/" <> header) dirs
    result <-
        runIO (lookupEnv "BINDGEN_EXTRA_CLANG_ARGS") >>= \case
            Just args -> findHeader $ includeDirs args
            Nothing -> findHeader ["/usr/include"]
    incDir <- case result of
        Just d -> pure d
        Nothing -> error $ "Could not find " <> header <> ". Set BINDGEN_EXTRA_CLANG_ARGS or install linux headers."
    generateCodes $ incDir <> "/" <> header
 )
