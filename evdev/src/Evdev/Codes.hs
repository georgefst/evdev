{-# LANGUAGE TemplateHaskell #-}

-- | Datatypes corresponding to the constants in [input-event-codes.h](https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h).
-- See [the Linux Kernel documentation](https://www.kernel.org/doc/html/latest/input/event-codes.html) for full details, noting that all names have been mechanically transformed into CamelCase.
module Evdev.Codes where

import Evdev.Codes.Generator
import Evdev.Raw
import Util

$(generateCodes)
