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

import HsBindgen.Runtime.LibC qualified
import HsBindgen.TH

do
    withHsBindgen
        def
            { clang =
                def
                    { extraIncludeDirs =
                        [ Dir "/nix/store/iqs23in0fqnf44vnb8l98x7bai77jiv3-libevdev-1.13.4/include"
                        , Dir "/nix/store/iqs23in0fqnf44vnb8l98x7bai77jiv3-libevdev-1.13.4/include/libevdev-1.0"
                        , Dir "/nix/store/7iwv8dcgsjmkrnn752hnfdxh3f7wahmd-linux-headers-6.16.7/include"
                        , Dir "/nix/store/gi4cz4ir3zlwhf1azqfgxqdnczfrwsr7-glibc-2.40-66-dev/include"
                        , Dir "/nix/store/kl4w4f8bb77faahsdv40gjmfzg2d081d-clang-21.1.2-lib/lib/clang/21/include"
                        ]
                    }
            , fieldNamingStrategy = OmitFieldPrefixes
            , programSlicing = EnableProgramSlicing
            }
        def
        do
            hashInclude "libevdev-1.0/libevdev/libevdev.h"
            hashInclude "libevdev-1.0/libevdev/libevdev-uinput.h"
            hashInclude "linux/input.h"
            hashInclude "linux/input-event-codes.h"
