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
    -- hs-bindgen uses its own libclang to parse C headers, which is entirely
    -- separate from Cabal's C compilation pipeline. Cabal's `pkgconfig-depends`
    -- feeds into GHC/cc but NOT into hs-bindgen's libclang. So we must provide
    -- include paths explicitly.
    --
    -- System headers (libc, linux): On non-Nix systems, libclang finds these
    -- via its default search paths (e.g. /usr/include). On Nix, the
    -- `hsBindgenHook` setup hook populates `BINDGEN_EXTRA_CLANG_ARGS` with the
    -- necessary `-isystem` flags.
    -- See: https://github.com/well-typed/hs-bindgen/tree/main/nix/
    --
    -- libevdev headers: These live in a versioned subdirectory (e.g.
    -- include/libevdev-1.0/) that neither libclang's defaults nor the Nix hook
    -- cover, so we always need pkg-config to locate them.
    libevdev <-
        dropWhileEnd isSpace
            . fromMaybe (error "pkg-config failed to locate libevdev")
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
