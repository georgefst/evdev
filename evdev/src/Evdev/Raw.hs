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

import Foreign
import HsBindgen.Runtime.LibC qualified
import HsBindgen.TH

do
    withHsBindgen
        def
            { clang =
                def
                    { extraIncludeDirs =
                        [ Dir "/nix/store/iqs23in0fqnf44vnb8l98x7bai77jiv3-libevdev-1.13.4/include/libevdev-1.0"
                        , Dir "/nix/store/gi4cz4ir3zlwhf1azqfgxqdnczfrwsr7-glibc-2.40-66-dev/include"
                        ]
                    }
            , fieldNamingStrategy = OmitFieldPrefixes
            , programSlicing = EnableProgramSlicing
            }
        def
        do
            hashInclude "libevdev/libevdev.h"
            hashInclude "libevdev/libevdev-uinput.h"

foreign import ccall "&libevdev_hs_close" libevdev_hs_close :: FinalizerPtr Libevdev
foreign import ccall "&libevdev_uinput_destroy" finalizer_libevdev_uinput_destroy :: FunPtr (Ptr Libevdev_uinput -> IO ())
