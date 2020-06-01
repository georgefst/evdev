module Evdev.LowLevel where

import Control.Monad (join)
import Control.Monad.Loops (iterateWhile)
import Data.ByteString (ByteString,packCString,useAsCString)
import Data.Coerce (coerce)
import Data.Int (Int32,Int64)
import Data.Word (Word16)
import Foreign (Ptr,allocaBytes,mallocBytes,mallocForeignPtrBytes,newForeignPtr_,nullPtr,peek,withForeignPtr)
import Foreign.C (CInt(..),CLong(..),CUInt(..),CUShort(..),CString)
import Foreign.C.Error (Errno(Errno))
import System.Posix.ByteString (RawFilePath)
import System.Posix.IO.ByteString (OpenMode(ReadOnly),defaultFileFlags,openFd)
import System.Posix.Types (Fd(Fd))

import Evdev.Codes

#include <errno.h>
#include <libevdev-1.0/libevdev/libevdev.h>
#include <libevdev-1.0/libevdev/libevdev-uinput.h>
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

{#pointer *libevdev_uinput as UDevice foreign finalizer libevdev_uinput_destroy newtype #}

--TODO '{#enum libevdev_uinput_open_mode {} #}' results in malformed output - c2hs bug
{#enum libevdev_uinput_open_mode as UInputOpenMode {LIBEVDEV_UINPUT_OPEN_MANAGED as UOMManaged} #}


data CEvent = CEvent
    { cEventType :: Word16
    , cEventCode :: Word16
    , cEventValue :: Int32
    , cEventTime :: CTimeVal
    }
    deriving (Eq, Ord, Read, Show)

data CTimeVal = CTimeVal
    { tvSec :: Int64
    , tvUsec :: Int64
    }
    deriving (Eq, Ord, Read, Show)


{- Complex stuff -}

{#fun libevdev_next_event { `Device', `CUInt', `Ptr ()' } -> `Errno' Errno #}
nextEvent :: Device -> CUInt -> IO (Errno, CEvent)
nextEvent dev flags = allocaBytes {#sizeof input_event #} $ \evPtr ->
    (,) <$> iterateWhile (== Errno (-{#const EAGAIN #})) (libevdev_next_event dev flags evPtr)
        <*> ( CEvent
            <$> (coerce <$> {#get input_event->type #} evPtr)
            <*> (coerce <$> {#get input_event->code #} evPtr)
            <*> (coerce <$> {#get input_event->value #} evPtr)
            <*> ( CTimeVal
                <$> (coerce <$> {#get input_event->time.tv_sec #} evPtr)
                <*> (coerce <$> {#get input_event->time.tv_usec #} evPtr)
            )
        )

{#fun libevdev_grab { `Device', `GrabMode' } -> `Errno' Errno #}
grabDevice :: Device -> GrabMode -> IO Errno
grabDevice = libevdev_grab

--TODO use 'libevdev_new_from_fd' when https://github.com/haskell/c2hs/issues/236 fixed
{#fun libevdev_new {} -> `Device' #}
{#fun libevdev_set_fd { `Device', unFd `Fd' } -> `Errno' Errno #}
newDevice :: RawFilePath -> IO (Errno, Device)
newDevice path = do
    fd <- openFd path ReadOnly Nothing defaultFileFlags
    dev <- libevdev_new
    err <- libevdev_set_fd dev fd
    return (err, dev)

--TODO 'useAsCString' copies, which seems unnecessary due to the 'const' in the C function
{#fun libevdev_set_name { `Device', `CString' } -> `()' #}
setDeviceName :: Device -> ByteString -> IO ()
setDeviceName dev name = useAsCString name $ libevdev_set_name dev

--TODO c2hs can't seem to help us here due to the nested pointer
foreign import ccall safe "Evdev/LowLevel.chs.h libevdev_uinput_create_from_device"
  libevdev_uinput_create_from_device :: Ptr Device -> CInt -> Ptr (Ptr UDevice) -> IO CInt
createFromDevice :: Device -> Fd -> IO (Errno, UDevice)
createFromDevice dev (Fd fd) = withDevice dev $ \devP -> do
    devFPP <- mallocForeignPtrBytes 0
    (e,x) <- withForeignPtr devFPP $ \devPP ->
        (,) <$> libevdev_uinput_create_from_device devP fd devPP <*> peek devPP
    devFP <- newForeignPtr_ x
    return (Errno e, UDevice devFP)

--TODO since the same technique produces just one 'IO' for  'deviceName', is this another c2hs bug?
{#fun libevdev_uinput_get_syspath  { `UDevice' } -> `IO (Maybe ByteString)' packCString' #}
getSyspath :: UDevice -> IO (Maybe ByteString)
getSyspath = join . libevdev_uinput_get_syspath
{#fun libevdev_uinput_get_devnode  { `UDevice' } -> `IO (Maybe ByteString)' packCString' #}
getDevnode :: UDevice -> IO (Maybe ByteString)
getDevnode = join . libevdev_uinput_get_devnode

-- val, min, max
--TODO what happens to the fields I haven't set?
withAbsInfo :: Int32 -> Int32 -> Int32 -> (Ptr () -> IO a) -> IO a
withAbsInfo v l u f = do
    p <- mallocBytes {#sizeof input_absinfo#}
    {#set input_absinfo.value#} p $ CInt v
    {#set input_absinfo.minimum#} p $ CInt l
    {#set input_absinfo.maximum#} p $ CInt u
    pf <- newForeignPtr_ p
    withForeignPtr pf f


{- Simpler functions -}

{#fun libevdev_has_property as hasProperty { `Device', devPropToInt `DeviceProperty' } -> `Bool' #}
{#fun libevdev_get_fd as deviceFd { `Device' } -> `Fd' Fd #}
{#fun libevdev_get_name as deviceName { `Device' } -> `IO ByteString' packCString #}
{#fun libevdev_enable_event_type as enableType { `Device', `Word16' } -> `Errno' Errno #}
{#fun libevdev_enable_event_code as enableCode { `Device', `Word16', `Word16', `Ptr ()' } -> `Errno' Errno #}
{#fun libevdev_uinput_write_event as writeEvent { `UDevice', `Word16', `Word16', `Int32' } -> `Errno' Errno #}


{- Util -}

devPropToInt :: DeviceProperty -> CUInt
devPropToInt = fromIntegral . fromEnum

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

unFd :: Fd -> CInt
unFd (Fd n) = n

handleNull :: b -> (Ptr a -> b) -> Ptr a -> b
handleNull def f p = if p == nullPtr then def else f p

packCString' :: CString -> IO (Maybe ByteString)
packCString' = handleNull (return Nothing) (fmap Just . packCString)
