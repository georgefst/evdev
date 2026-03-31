module Evdev.LowLevel where

import Data.ByteString (ByteString, packCString, useAsCString)
import Data.Int (Int32)
import Data.Word (Word16)
import Foreign (ForeignPtr, FunPtr, Ptr, newForeignPtr, nullPtr, withForeignPtr)
import Foreign.C (CString, Errno (Errno))
import Foreign.C.ConstPtr (ConstPtr (..))
import System.Posix.Types (Fd (Fd))

import Evdev.Codes
import Evdev.Raw qualified as Raw

-- * Device lifecycle

foreign import ccall "&libevdev_hs_close" finalizer_libevdev_hs_close :: FunPtr (Ptr Raw.Libevdev -> IO ())
foreign import ccall "&libevdev_uinput_destroy" finalizer_libevdev_uinput_destroy :: FunPtr (Ptr Raw.Libevdev_uinput -> IO ())

libevdev_new :: IO (ForeignPtr Raw.Libevdev)
libevdev_new = do
    ptr <- Raw.libevdev_new
    fp <- newForeignPtr finalizer_libevdev_hs_close ptr
    pure fp

libevdev_set_fd :: ForeignPtr Raw.Libevdev -> Fd -> IO Errno
libevdev_set_fd dev (Fd fd) = withForeignPtr dev $ \devPtr ->
    Errno <$> Raw.libevdev_set_fd devPtr fd

newDeviceFromFd :: Fd -> IO (Errno, ForeignPtr Raw.Libevdev)
newDeviceFromFd fd = do
    dev <- libevdev_new
    err <- libevdev_set_fd dev fd
    pure (err, dev)

-- * Device properties (getters)

deviceFd :: ForeignPtr Raw.Libevdev -> IO Fd
deviceFd dev = withForeignPtr dev $ \devPtr ->
    Fd <$> Raw.libevdev_get_fd (ConstPtr devPtr)

deviceName :: ForeignPtr Raw.Libevdev -> IO (IO ByteString)
deviceName dev = withForeignPtr dev $ \devPtr -> do
    cstr <- Raw.libevdev_get_name (ConstPtr devPtr)
    pure $ packCString (unConstPtr cstr)

devicePhys :: ForeignPtr Raw.Libevdev -> IO (IO (Maybe ByteString))
devicePhys dev = withForeignPtr dev $ \devPtr -> do
    cstr <- Raw.libevdev_get_phys (ConstPtr devPtr)
    pure $ packCString' (unConstPtr cstr)

deviceUniq :: ForeignPtr Raw.Libevdev -> IO (IO (Maybe ByteString))
deviceUniq dev = withForeignPtr dev $ \devPtr -> do
    cstr <- Raw.libevdev_get_uniq (ConstPtr devPtr)
    pure $ packCString' (unConstPtr cstr)

deviceProduct :: ForeignPtr Raw.Libevdev -> IO Int
deviceProduct dev = withForeignPtr dev $ \devPtr ->
    fromIntegral <$> Raw.libevdev_get_id_product (ConstPtr devPtr)

deviceVendor :: ForeignPtr Raw.Libevdev -> IO Int
deviceVendor dev = withForeignPtr dev $ \devPtr ->
    fromIntegral <$> Raw.libevdev_get_id_vendor (ConstPtr devPtr)

deviceBustype :: ForeignPtr Raw.Libevdev -> IO Int
deviceBustype dev = withForeignPtr dev $ \devPtr ->
    fromIntegral <$> Raw.libevdev_get_id_bustype (ConstPtr devPtr)

deviceVersion :: ForeignPtr Raw.Libevdev -> IO Int
deviceVersion dev = withForeignPtr dev $ \devPtr ->
    fromIntegral <$> Raw.libevdev_get_id_version (ConstPtr devPtr)

-- * Device properties (setters)

setDeviceName :: ForeignPtr Raw.Libevdev -> ByteString -> IO ()
setDeviceName dev name = withForeignPtr dev $ \devPtr ->
    useAsCString name $ \cstr -> Raw.libevdev_set_name devPtr (ConstPtr cstr)

setDevicePhys :: ForeignPtr Raw.Libevdev -> ByteString -> IO ()
setDevicePhys dev phys = withForeignPtr dev $ \devPtr ->
    useAsCString phys $ \cstr -> Raw.libevdev_set_phys devPtr (ConstPtr cstr)

setDeviceUniq :: ForeignPtr Raw.Libevdev -> ByteString -> IO ()
setDeviceUniq dev uniq = withForeignPtr dev $ \devPtr ->
    useAsCString uniq $ \cstr -> Raw.libevdev_set_uniq devPtr (ConstPtr cstr)

libevdev_set_id_product :: ForeignPtr Raw.Libevdev -> Int -> IO ()
libevdev_set_id_product dev n = withForeignPtr dev $ \devPtr ->
    Raw.libevdev_set_id_product devPtr (fromIntegral n)

libevdev_set_id_vendor :: ForeignPtr Raw.Libevdev -> Int -> IO ()
libevdev_set_id_vendor dev n = withForeignPtr dev $ \devPtr ->
    Raw.libevdev_set_id_vendor devPtr (fromIntegral n)

libevdev_set_id_bustype :: ForeignPtr Raw.Libevdev -> Int -> IO ()
libevdev_set_id_bustype dev n = withForeignPtr dev $ \devPtr ->
    Raw.libevdev_set_id_bustype devPtr (fromIntegral n)

libevdev_set_id_version :: ForeignPtr Raw.Libevdev -> Int -> IO ()
libevdev_set_id_version dev n = withForeignPtr dev $ \devPtr ->
    Raw.libevdev_set_id_version devPtr (fromIntegral n)

-- * Capability queries

hasProperty :: ForeignPtr Raw.Libevdev -> DeviceProperty -> IO Bool
hasProperty dev prop = withForeignPtr dev $ \devPtr ->
    (/= 0) <$> Raw.libevdev_has_property (ConstPtr devPtr) (convertEnum prop)

hasEventType :: ForeignPtr Raw.Libevdev -> EventType -> IO Bool
hasEventType dev et = withForeignPtr dev $ \devPtr ->
    (/= 0) <$> Raw.libevdev_has_event_type (ConstPtr devPtr) (convertEnum et)

hasEventCode :: ForeignPtr Raw.Libevdev -> Word16 -> Word16 -> IO Bool
hasEventCode dev t c = withForeignPtr dev $ \devPtr ->
    (/= 0) <$> Raw.libevdev_has_event_code (ConstPtr devPtr) (fromIntegral t) (fromIntegral c)

-- * Uinput

getSyspath :: ForeignPtr Raw.Libevdev_uinput -> IO (Maybe ByteString)
getSyspath dev = withForeignPtr dev $ \devPtr ->
    Raw.libevdev_uinput_get_syspath devPtr >>= packCString' . unConstPtr

getDevnode :: ForeignPtr Raw.Libevdev_uinput -> IO (Maybe ByteString)
getDevnode dev = withForeignPtr dev $ \devPtr ->
    Raw.libevdev_uinput_get_devnode devPtr >>= packCString' . unConstPtr

writeEvent :: ForeignPtr Raw.Libevdev_uinput -> Word16 -> Word16 -> Int32 -> IO Errno
writeEvent dev t c v = withForeignPtr dev $ \devPtr ->
    Errno <$> Raw.libevdev_uinput_write_event (ConstPtr devPtr) (fromIntegral t) (fromIntegral c) (fromIntegral v)

-- * Util

convertEnum :: (Enum a, Integral b) => a -> b
convertEnum = fromIntegral . fromEnum

handleNull :: b -> (Ptr a -> b) -> Ptr a -> b
handleNull def f p = if p == nullPtr then def else f p

packCString' :: CString -> IO (Maybe ByteString)
packCString' = handleNull (return Nothing) (fmap Just . packCString)
