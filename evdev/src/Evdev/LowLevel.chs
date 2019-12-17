module Evdev.LowLevel where

import Control.Monad.Loops (iterateWhile)
import Data.Int (Int16,Int32)
import Data.Time.Clock (DiffTime,picosecondsToDiffTime)
import Foreign (Ptr)
import Foreign.C (CInt,CUInt,CLong)
import Foreign.C.Error (Errno(Errno))
import Foreign.ForeignPtr (mallocForeignPtrBytes,withForeignPtr)
import Foreign.Storable (peekByteOff)
import System.Posix.ByteString (RawFilePath)
import System.Posix.IO.ByteString (OpenMode(ReadOnly),defaultFileFlags,openFd)
import System.Posix.Types (Fd(Fd))

import Evdev.Codes

#include <errno.h>
#include <libevdev-1.0/libevdev/libevdev.h>
#include <linux/input.h>

{#enum libevdev_read_flag as ReadFlags {
    LIBEVDEV_READ_FLAG_SYNC as Sync,
    LIBEVDEV_READ_FLAG_NORMAL as Normal,
    LIBEVDEV_READ_FLAG_FORCE_SYNC as ForceSync,
    LIBEVDEV_READ_FLAG_BLOCKING as Blocking }
    deriving (Eq,Ord,Show) #}

{#enum libevdev_grab_mode as GrabMode { underscoreToCase } deriving (Show) #}

{#pointer *input_event as Event foreign newtype#}

{#pointer *timeval as Time foreign newtype#}

{#pointer *libevdev as Device newtype #}


{- Conversions -}

convertEvent :: Event -> IO (Int,Int16,Int32,DiffTime)
convertEvent ev = (,,,)
    <$> getIntField {#get input_event->type #}
    <*> getIntField {#get input_event->code #}
    <*> getIntField {#get input_event->value #}
    <*> withEvent ev getTime
    where
        convertTime s us = picosecondsToDiffTime $ 1000000000000 * fromIntegral s + 1000000 * fromIntegral us
        getIntField :: (Integral a,Integral b) => (Ptr Event -> IO a) -> IO b
        getIntField f = withEvent ev (fmap fromIntegral . f)
        getTime :: Ptr Event -> IO DiffTime
        getTime ptr =
            let sec, usec :: IO CLong
                sec = peekByteOff ptr 0
-- ideally, should use field alias, but: https://github.com/haskell/c2hs/issues/244
#if defined(__FreeBSD__)
                usec = peekByteOff ptr {#offsetof input_event->time.tv_usec #}
#else
                usec = peekByteOff ptr {#offsetof input_event->__usec #}
#endif
            in  convertTime <$> sec <*> usec

nextEvent :: Device -> CUInt -> IO (Errno, Event)
nextEvent dev flags = iterateWhile ((== Errno (-{#const EAGAIN #})) . fst) $ do
    ptr <- mallocForeignPtrBytes {#sizeof input_event #}
    err <- withForeignPtr ptr $ {#call libevdev_next_event #} dev flags
    return (Errno err, Event ptr)

{#fun libevdev_grab { `Device', `GrabMode' } -> `CInt' #}
grabDevice :: Device -> GrabMode -> IO (Errno, ())
grabDevice dev mode = do
    err <- libevdev_grab dev mode
    return (Errno err, ())

newDevice :: RawFilePath -> IO (Errno, Device)
newDevice path = do
    Fd n <- openFd path ReadOnly Nothing $ defaultFileFlags
    dev <- {#call libevdev_new #}
    err <- {#call libevdev_set_fd #} dev n
    return (Errno err, dev)


{- Simpler functions -}

{#fun libevdev_has_property as hasProperty { `Device', convertEnum `DeviceProperty' } -> `Bool' #}
{#fun libevdev_get_fd as deviceFd { `Device' } -> `Fd' Fd #}
{#fun libevdev_get_name as deviceName { `Device' } -> `String' #}
--TODO should really be ByteString


{- Util -}

convertEnum :: (Enum a, Integral b) => a -> b
convertEnum = fromIntegral . fromEnum
