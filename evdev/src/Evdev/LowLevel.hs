module Evdev.LowLevel where

import Data.ByteString (ByteString, packCString, useAsCString)
import Data.Int (Int32)
import Data.Word (Word16)
import Foreign (ForeignPtr, FunPtr, Ptr, castPtr, mallocForeignPtrBytes, newForeignPtr, nullPtr, peek, withForeignPtr)
import Foreign.C (CString)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Error (Errno (Errno))
import Foreign.Storable (sizeOf)
import System.Posix.Types (Fd (Fd))

import Evdev.Codes
import Evdev.Raw qualified as Raw

-- * Opaque device types

newtype Device = Device (ForeignPtr Raw.Libevdev)
newtype UDevice = UDevice (ForeignPtr Raw.Libevdev_uinput)

withDevice :: Device -> (Ptr Raw.Libevdev -> IO a) -> IO a
withDevice (Device fp) = withForeignPtr fp

withUDevice :: UDevice -> (Ptr Raw.Libevdev_uinput -> IO a) -> IO a
withUDevice (UDevice fp) = withForeignPtr fp

foreign import ccall "&libevdev_hs_close" finalizer_libevdev_hs_close :: FunPtr (Ptr Raw.Libevdev -> IO ())
foreign import ccall "&libevdev_uinput_destroy" finalizer_libevdev_uinput_destroy :: FunPtr (Ptr Raw.Libevdev_uinput -> IO ())

-- * Device lifecycle

libevdev_new :: IO Device
libevdev_new = do
    ptr <- Raw.libevdev_new
    fp <- newForeignPtr finalizer_libevdev_hs_close ptr
    pure (Device fp)

libevdev_set_fd :: Device -> Fd -> IO Errno
libevdev_set_fd dev (Fd fd) = withDevice dev $ \devPtr ->
    Errno <$> Raw.libevdev_set_fd devPtr fd

newDeviceFromFd :: Fd -> IO (Errno, Device)
newDeviceFromFd fd = do
    dev <- libevdev_new
    err <- libevdev_set_fd dev fd
    pure (err, dev)

-- * Device properties (getters)

deviceFd :: Device -> IO Fd
deviceFd dev = withDevice dev $ \devPtr ->
    Fd <$> Raw.libevdev_get_fd (ConstPtr devPtr)

deviceName :: Device -> IO (IO ByteString)
deviceName dev = withDevice dev $ \devPtr -> do
    cstr <- Raw.libevdev_get_name (ConstPtr devPtr)
    pure $ packCString (unConstPtr cstr)

devicePhys :: Device -> IO (IO (Maybe ByteString))
devicePhys dev = withDevice dev $ \devPtr -> do
    cstr <- Raw.libevdev_get_phys (ConstPtr devPtr)
    pure $ packCString' (unConstPtr cstr)

deviceUniq :: Device -> IO (IO (Maybe ByteString))
deviceUniq dev = withDevice dev $ \devPtr -> do
    cstr <- Raw.libevdev_get_uniq (ConstPtr devPtr)
    pure $ packCString' (unConstPtr cstr)

deviceProduct :: Device -> IO Int
deviceProduct dev = withDevice dev $ \devPtr ->
    fromIntegral <$> Raw.libevdev_get_id_product (ConstPtr devPtr)

deviceVendor :: Device -> IO Int
deviceVendor dev = withDevice dev $ \devPtr ->
    fromIntegral <$> Raw.libevdev_get_id_vendor (ConstPtr devPtr)

deviceBustype :: Device -> IO Int
deviceBustype dev = withDevice dev $ \devPtr ->
    fromIntegral <$> Raw.libevdev_get_id_bustype (ConstPtr devPtr)

deviceVersion :: Device -> IO Int
deviceVersion dev = withDevice dev $ \devPtr ->
    fromIntegral <$> Raw.libevdev_get_id_version (ConstPtr devPtr)

-- * Device properties (setters)

setDeviceName :: Device -> ByteString -> IO ()
setDeviceName dev name = withDevice dev $ \devPtr ->
    useAsCString name $ \cstr -> Raw.libevdev_set_name devPtr (ConstPtr cstr)

setDevicePhys :: Device -> ByteString -> IO ()
setDevicePhys dev phys = withDevice dev $ \devPtr ->
    useAsCString phys $ \cstr -> Raw.libevdev_set_phys devPtr (ConstPtr cstr)

setDeviceUniq :: Device -> ByteString -> IO ()
setDeviceUniq dev uniq = withDevice dev $ \devPtr ->
    useAsCString uniq $ \cstr -> Raw.libevdev_set_uniq devPtr (ConstPtr cstr)

libevdev_set_id_product :: Device -> Int -> IO ()
libevdev_set_id_product dev n = withDevice dev $ \devPtr ->
    Raw.libevdev_set_id_product devPtr (fromIntegral n)

libevdev_set_id_vendor :: Device -> Int -> IO ()
libevdev_set_id_vendor dev n = withDevice dev $ \devPtr ->
    Raw.libevdev_set_id_vendor devPtr (fromIntegral n)

libevdev_set_id_bustype :: Device -> Int -> IO ()
libevdev_set_id_bustype dev n = withDevice dev $ \devPtr ->
    Raw.libevdev_set_id_bustype devPtr (fromIntegral n)

libevdev_set_id_version :: Device -> Int -> IO ()
libevdev_set_id_version dev n = withDevice dev $ \devPtr ->
    Raw.libevdev_set_id_version devPtr (fromIntegral n)

-- * Capability queries

hasProperty :: Device -> DeviceProperty -> IO Bool
hasProperty dev prop = withDevice dev $ \devPtr ->
    (/= 0) <$> Raw.libevdev_has_property (ConstPtr devPtr) (convertEnum prop)

hasEventType :: Device -> EventType -> IO Bool
hasEventType dev et = withDevice dev $ \devPtr ->
    (/= 0) <$> Raw.libevdev_has_event_type (ConstPtr devPtr) (convertEnum et)

hasEventCode :: Device -> Word16 -> Word16 -> IO Bool
hasEventCode dev t c = withDevice dev $ \devPtr ->
    (/= 0) <$> Raw.libevdev_has_event_code (ConstPtr devPtr) (fromIntegral t) (fromIntegral c)

-- * Uinput

createFromDevice :: Device -> Fd -> IO (Errno, UDevice)
createFromDevice dev (Fd fd) = withDevice dev $ \devPtr -> do
    udevPtrPtr <- mallocForeignPtrBytes (sizeOf (undefined :: Ptr ()))
    (e, udevPtr) <- withForeignPtr udevPtrPtr $ \pp ->
        (,) <$> Raw.libevdev_uinput_create_from_device (ConstPtr devPtr) fd (castPtr pp) <*> peek pp
    udevFP <- newForeignPtr finalizer_libevdev_uinput_destroy udevPtr
    pure (Errno e, UDevice udevFP)

getSyspath :: UDevice -> IO (Maybe ByteString)
getSyspath dev = withUDevice dev $ \devPtr ->
    Raw.libevdev_uinput_get_syspath devPtr >>= packCString' . unConstPtr

getDevnode :: UDevice -> IO (Maybe ByteString)
getDevnode dev = withUDevice dev $ \devPtr ->
    Raw.libevdev_uinput_get_devnode devPtr >>= packCString' . unConstPtr

writeEvent :: UDevice -> Word16 -> Word16 -> Int32 -> IO Errno
writeEvent dev t c v = withUDevice dev $ \devPtr ->
    Errno <$> Raw.libevdev_uinput_write_event (ConstPtr devPtr) (fromIntegral t) (fromIntegral c) (fromIntegral v)

-- * Util

convertEnum :: (Enum a, Integral b) => a -> b
convertEnum = fromIntegral . fromEnum

handleNull :: b -> (Ptr a -> b) -> Ptr a -> b
handleNull def f p = if p == nullPtr then def else f p

packCString' :: CString -> IO (Maybe ByteString)
packCString' = handleNull (return Nothing) (fmap Just . packCString)
