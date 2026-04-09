{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Evdev.Raw where

import Data.Char
import Data.List
import Data.Maybe
import Foreign
import HsBindgen.Runtime.LibC qualified
import HsBindgen.TH
import Language.Haskell.TH
import System.Process

do
    libevdev <-
        dropWhileEnd isSpace
            . fromMaybe (error "bad pkg-config response")
            . stripPrefix "-I"
            <$> runIO (readProcess "pkg-config" ["--cflags-only-I", "libevdev"] "")
    withHsBindgen
        def
            { clang = def{extraIncludeDirs = [Dir libevdev]}
            , fieldNamingStrategy = OmitFieldPrefixes
            , programSlicing = EnableProgramSlicing
            }
        def
            { categoryChoice =
                def
                    { cUnsafe = ExcludeCategory
                    , cFunPtr = IncludeTermCategory $ RenameTerm (<> "_funptr")
                    }
            }
        do
            hashInclude "libevdev/libevdev.h"
            hashInclude "libevdev/libevdev-uinput.h"
            hashInclude "linux/input-event-codes.h"

foreign import ccall "&libevdev_hs_close" libevdev_hs_close :: FinalizerPtr Libevdev
