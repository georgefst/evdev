module Evdev.LowLevel where

import Control.Monad.Loops (iterateWhile)
import Data.ByteString (ByteString,packCString)
import Foreign (Ptr,allocaBytes)
import Foreign.C (CInt(..),CUInt(..),CUShort(..),CLong)
import Foreign.C.Error (Errno(Errno))
import System.Posix.ByteString (RawFilePath)
import System.Posix.IO.ByteString (OpenMode(ReadOnly),defaultFileFlags,openFd)
import System.Posix.Types (Fd(Fd))

import Evdev.Codes

#include <errno.h>
#include <libevdev-1.0/libevdev/libevdev.h>
#include <linux/input.h>

{#enum libevdev_read_flag as ReadFlag {
    LIBEVDEV_READ_FLAG_SYNC as Sync,
    LIBEVDEV_READ_FLAG_NORMAL as Normal,
    LIBEVDEV_READ_FLAG_FORCE_SYNC as ForceSync,
    LIBEVDEV_READ_FLAG_BLOCKING as Blocking }
    deriving (Eq,Ord,Show) #}

{#enum libevdev_grab_mode as GrabMode { underscoreToCase } deriving (Show) #}

{#pointer *libevdev as Device foreign finalizer libevdev_hs_close newtype #}
--TODO any reason c2hs doesn't allow a haskell function as the finalizer?
    -- failing that, any reason not to have actual inline c?
--TODO expose this directly, seeing as the GC makes no guarantees of promptness
#c
void libevdev_hs_close(struct libevdev *dev);
#endc


{- Conversions -}

{#fun libevdev_next_event { `Device', `CUInt', `Ptr ()' } -> `Errno' Errno #}
nextEvent :: Device -> CUInt -> IO (Errno, (CUShort,CUShort,CInt,CLong,CLong)) --TODO not CLong on all platforms
nextEvent dev flags = allocaBytes {#sizeof input_event #} $ \evPtr ->
    (,) <$> iterateWhile (== Errno (-{#const EAGAIN #})) (libevdev_next_event dev flags evPtr)
        <*> (
            (,,,,)
            <$> {#get input_event->type #} evPtr
            <*> {#get input_event->code #} evPtr
            <*> {#get input_event->value #} evPtr
            <*> {#get input_event->time.tv_sec #} evPtr
            <*> {#get input_event->time.tv_usec #} evPtr
        )

{#fun libevdev_grab { `Device', `GrabMode' } -> `Errno' Errno #}
grabDevice :: Device -> GrabMode -> IO (Errno, ())
grabDevice = fmap (,()) .: libevdev_grab

--TODO use 'libevdev_new_from_fd' when https://github.com/haskell/c2hs/issues/236 fixed
{#fun libevdev_new {} -> `Device' #}
{#fun libevdev_set_fd { `Device', unFd `Fd' } -> `Errno' Errno #}
newDevice :: RawFilePath -> IO (Errno, Device)
newDevice path = do
    fd <- openFd path ReadOnly Nothing defaultFileFlags
    dev <- libevdev_new
    err <- libevdev_set_fd dev fd
    return (err, dev)


{- Simpler functions -}

{#fun libevdev_has_property as hasProperty { `Device', convertEnum `DeviceProperty' } -> `Bool' #}
{#fun libevdev_get_fd as deviceFd { `Device' } -> `Fd' Fd #}
{#fun libevdev_get_name as deviceName { `Device' } -> `IO ByteString' packCString #}

{- Util -}

convertEnum :: DeviceProperty -> CUInt
convertEnum = fromIntegral . fromEnum

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

unFd :: Fd -> CInt
unFd (Fd n) = n
