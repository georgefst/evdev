module Evdev.LowLevel where

import Control.Monad (join)
import Data.ByteString (ByteString,packCString,useAsCString)
import Data.Coerce (coerce)
import Data.Int (Int32,Int64)
import Data.Word (Word16, Word32)
import Foreign (Ptr,allocaBytes,mallocBytes,mallocForeignPtrBytes,newForeignPtr_,nullPtr,peek,withForeignPtr)
import Foreign.C (CInt(..),CLong(..),CUInt(..),CUShort(..),CString)
import Foreign.C.Error (Errno(Errno), eOK, eAGAIN)
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
    (,) <$> libevdev_next_event dev flags evPtr <*> getEvent evPtr
nextEventMay :: Device -> CUInt -> IO (Errno, Maybe CEvent)
nextEventMay dev flags = allocaBytes {#sizeof input_event #} $ \evPtr -> do
    err <- libevdev_next_event dev flags evPtr
    if err /= eOK
        then return
            ( if negateErrno err == eAGAIN then eOK else err
            , Nothing
            )
        else (eOK,) . Just <$> getEvent evPtr
getEvent :: Ptr () -> IO CEvent
getEvent evPtr = CEvent
    <$> (coerce <$> {#get input_event->type #} evPtr)
    <*> (coerce <$> {#get input_event->code #} evPtr)
    <*> (coerce <$> {#get input_event->value #} evPtr)
    <*> ( CTimeVal
        <$> (coerce <$> {#get input_event->time.tv_sec #} evPtr)
        <*> (coerce <$> {#get input_event->time.tv_usec #} evPtr)
        )

{#fun libevdev_grab { `Device', `GrabMode' } -> `Errno' Errno #}
grabDevice :: Device -> GrabMode -> IO Errno
grabDevice = libevdev_grab

--TODO use 'libevdev_new_from_fd' when https://github.com/haskell/c2hs/issues/236 fixed
{#fun libevdev_new {} -> `Device' #}
{#fun libevdev_set_fd { `Device', unFd `Fd' } -> `Errno' Errno #}
newDeviceFromFd :: Fd -> IO (Errno, Device)
newDeviceFromFd fd = libevdev_new >>= \dev -> (, dev) <$> libevdev_set_fd dev fd

--TODO 'useAsCString' copies, which seems unnecessary due to the 'const' in the C function
{#fun libevdev_set_name { `Device', `CString' } -> `()' #}
setDeviceName :: Device -> ByteString -> IO ()
setDeviceName dev name = useAsCString name $ libevdev_set_name dev
{#fun libevdev_set_phys { `Device', `CString' } -> `()' #}
setDevicePhys :: Device -> ByteString -> IO ()
setDevicePhys dev phys = useAsCString phys $ libevdev_set_phys dev
{#fun libevdev_set_uniq { `Device', `CString' } -> `()' #}
setDeviceUniq :: Device -> ByteString -> IO ()
setDeviceUniq dev uniq = useAsCString uniq $ libevdev_set_uniq dev

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

data AbsInfo = AbsInfo
    { absValue :: Int32
    , absMinimum :: Int32
    , absMaximum :: Int32
    , absFuzz :: Int32
    , absFlat :: Int32
    , absResolution :: Int32
    }
    deriving (Show)
withAbsInfo :: AbsInfo -> (Ptr () -> IO a) -> IO a
withAbsInfo AbsInfo{..} f = do
    p <- mallocBytes {#sizeof input_absinfo#}
    {#set input_absinfo.value#} p $ CInt absValue
    {#set input_absinfo.minimum#} p $ CInt absMinimum
    {#set input_absinfo.maximum#} p $ CInt absMaximum
    {#set input_absinfo.fuzz#} p $ CInt absFuzz
    {#set input_absinfo.flat#} p $ CInt absFlat
    {#set input_absinfo.resolution#} p $ CInt absResolution
    pf <- newForeignPtr_ p
    withForeignPtr pf f

--TODO can c2hs make this simpler at all?
foreign import ccall safe "Evdev/LowLevel.chs.h libevdev_get_abs_info"
  libevdev_get_abs_info :: Ptr Device -> CUInt -> IO (Ptr ())
getAbsInfo :: Device -> Word32 -> IO (Maybe AbsInfo)
getAbsInfo dev x = withDevice dev \devPtr ->
    libevdev_get_abs_info devPtr (CUInt x) >>= handleNull (pure Nothing) \absinfoPtr -> do
        CInt absValue <- {#get input_absinfo.value#} absinfoPtr
        CInt absMinimum <- {#get input_absinfo.minimum#} absinfoPtr
        CInt absMaximum <- {#get input_absinfo.maximum#} absinfoPtr
        CInt absFuzz <- {#get input_absinfo.fuzz#} absinfoPtr
        CInt absFlat <- {#get input_absinfo.flat#} absinfoPtr
        CInt absResolution <- {#get input_absinfo.resolution#} absinfoPtr
        pure $ Just AbsInfo{..}


{- Simpler functions -}

{#fun libevdev_has_property as hasProperty { `Device', convertEnum `DeviceProperty' } -> `Bool' #}
{#fun libevdev_has_event_type as hasEventType { `Device', convertEnum `EventType' } -> `Bool' #}
{#fun libevdev_has_event_code as hasEventCode { `Device', `Word16', `Word16' } -> `Bool' #}
{#fun libevdev_get_fd as deviceFd { `Device' } -> `Fd' Fd #}
{#fun libevdev_get_name as deviceName { `Device' } -> `IO ByteString' packCString #}
{#fun libevdev_get_phys as devicePhys { `Device' } -> `IO (Maybe ByteString)' packCString' #}
{#fun libevdev_get_uniq as deviceUniq { `Device' } -> `IO (Maybe ByteString)' packCString' #}
{#fun libevdev_get_id_product as deviceProduct { `Device' } -> `Int' #}
{#fun libevdev_get_id_vendor as deviceVendor { `Device' } -> `Int' #}
{#fun libevdev_get_id_bustype as deviceBustype { `Device' } -> `Int' #}
{#fun libevdev_get_id_version as deviceVersion { `Device' } -> `Int' #}
{#fun libevdev_set_id_product { `Device', `Int' } -> `()' #}
{#fun libevdev_set_id_vendor { `Device', `Int' } -> `()' #}
{#fun libevdev_set_id_bustype { `Device', `Int' } -> `()' #}
{#fun libevdev_set_id_version { `Device', `Int' } -> `()' #}
{#fun libevdev_enable_event_type as enableType { `Device', `Word16' } -> `Errno' Errno #}
{#fun libevdev_enable_event_code as enableCode { `Device', `Word16', `Word16', `Ptr ()' } -> `Errno' Errno #}
{#fun libevdev_uinput_write_event as writeEvent { `UDevice', `Word16', `Word16', `Int32' } -> `Errno' Errno #}

-- | LEDs values
{#enum define LEDValue {
    LIBEVDEV_LED_ON as LedOn,
    LIBEVDEV_LED_OFF as LedOff}
    deriving (Bounded, Eq, Ord, Read, Show) #}
{#fun libevdev_kernel_set_led_value { `Device', convertEnum `LEDEvent', `LEDValue' } -> `Errno' Errno #}

{- Util -}

convertEnum :: (Enum a, Integral b) => a -> b
convertEnum = fromIntegral . fromEnum

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

unFd :: Fd -> CInt
unFd (Fd n) = n

handleNull :: b -> (Ptr a -> b) -> Ptr a -> b
handleNull def f p = if p == nullPtr then def else f p

packCString' :: CString -> IO (Maybe ByteString)
packCString' = handleNull (return Nothing) (fmap Just . packCString)

negateErrno :: Errno -> Errno
negateErrno (Errno cint) = Errno (-cint)
