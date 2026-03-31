module Evdev.LowLevel where

import Data.ByteString (ByteString, packCString, useAsCString)
import Data.Int (Int32, Int64)
import Data.Word (Word16, Word32)
import Foreign (ForeignPtr, FunPtr, Ptr, allocaBytes, castPtr, mallocBytes, mallocForeignPtrBytes, newForeignPtr, newForeignPtr_, nullPtr, peek, poke, withForeignPtr)
import Foreign.C (CInt(..), CLong(..), CUInt(..), CUShort(..), CString)
import Foreign.C.ConstPtr (ConstPtr(..))
import Foreign.C.Error (Errno(Errno), eOK, eAGAIN)
import Foreign.Storable (sizeOf)
import System.Posix.Types (Fd(Fd))

import Evdev.Raw (Libevdev, Libevdev_uinput, Input_event(..), Input_absinfo(..), Timeval(..), C__U16(..), C__S32(..), C__Time_t(..), C__Suseconds_t(..))
import qualified Evdev.Raw as Raw
import Evdev.Codes (DeviceProperty, EventType, LEDEvent)

-- * Enums

-- | Extract an Int from an hs-bindgen enum newtype
rawEnum :: Integral a => a -> Int
rawEnum = fromIntegral

data ReadFlag = Sync | Normal | ForceSync | Blocking
    deriving (Eq, Ord, Show)
instance Enum ReadFlag where
    fromEnum Sync      = let Raw.Libevdev_read_flag n = Raw.LIBEVDEV_READ_FLAG_SYNC       in rawEnum n
    fromEnum Normal    = let Raw.Libevdev_read_flag n = Raw.LIBEVDEV_READ_FLAG_NORMAL      in rawEnum n
    fromEnum ForceSync = let Raw.Libevdev_read_flag n = Raw.LIBEVDEV_READ_FLAG_FORCE_SYNC  in rawEnum n
    fromEnum Blocking  = let Raw.Libevdev_read_flag n = Raw.LIBEVDEV_READ_FLAG_BLOCKING    in rawEnum n
    toEnum n
        | n == fromEnum Sync      = Sync
        | n == fromEnum Normal    = Normal
        | n == fromEnum ForceSync = ForceSync
        | n == fromEnum Blocking  = Blocking
        | otherwise = error $ "ReadFlag.toEnum: Cannot match " ++ show n

data GrabMode = LibevdevGrab | LibevdevUngrab
    deriving (Show)
instance Enum GrabMode where
    fromEnum LibevdevGrab   = let Raw.Libevdev_grab_mode n = Raw.LIBEVDEV_GRAB   in rawEnum n
    fromEnum LibevdevUngrab = let Raw.Libevdev_grab_mode n = Raw.LIBEVDEV_UNGRAB  in rawEnum n
    toEnum n
        | n == fromEnum LibevdevGrab   = LibevdevGrab
        | n == fromEnum LibevdevUngrab = LibevdevUngrab
        | otherwise = error $ "GrabMode.toEnum: Cannot match " ++ show n

data LEDValue = LedOn | LedOff
    deriving (Bounded, Eq, Ord, Read, Show)
instance Enum LEDValue where
    fromEnum LedOn  = let Raw.Libevdev_led_value n = Raw.LIBEVDEV_LED_ON  in rawEnum n
    fromEnum LedOff = let Raw.Libevdev_led_value n = Raw.LIBEVDEV_LED_OFF in rawEnum n
    toEnum n
        | n == fromEnum LedOn  = LedOn
        | n == fromEnum LedOff = LedOff
        | otherwise = error $ "LEDValue.toEnum: Cannot match " ++ show n

data UInputOpenMode = UOMManaged
    deriving (Show)
instance Enum UInputOpenMode where
    fromEnum UOMManaged = let Raw.Libevdev_uinput_open_mode n = Raw.LIBEVDEV_UINPUT_OPEN_MANAGED in rawEnum n
    toEnum n
        | n == fromEnum UOMManaged = UOMManaged
        | otherwise = error $ "UInputOpenMode.toEnum: Cannot match " ++ show n

grabModeToRaw :: GrabMode -> Raw.Libevdev_grab_mode
grabModeToRaw = \case
    LibevdevGrab   -> Raw.LIBEVDEV_GRAB
    LibevdevUngrab -> Raw.LIBEVDEV_UNGRAB

ledValueToRaw :: LEDValue -> Raw.Libevdev_led_value
ledValueToRaw = \case
    LedOn  -> Raw.LIBEVDEV_LED_ON
    LedOff -> Raw.LIBEVDEV_LED_OFF

-- * Opaque device types

newtype Device = Device (ForeignPtr Libevdev)
newtype UDevice = UDevice (ForeignPtr Libevdev_uinput)

withDevice :: Device -> (Ptr Libevdev -> IO a) -> IO a
withDevice (Device fp) = withForeignPtr fp

withUDevice :: UDevice -> (Ptr Libevdev_uinput -> IO a) -> IO a
withUDevice (UDevice fp) = withForeignPtr fp

foreign import ccall "&libevdev_hs_close" finalizer_libevdev_hs_close :: FunPtr (Ptr Libevdev -> IO ())
foreign import ccall "&libevdev_uinput_destroy" finalizer_libevdev_uinput_destroy :: FunPtr (Ptr Libevdev_uinput -> IO ())

-- | Convert a Ptr to a ConstPtr (for calling const-qualified C functions)
constPtr :: Ptr a -> ConstPtr a
constPtr = ConstPtr

-- | Convert a ConstPtr to a regular Ptr
unConstPtr' :: ConstPtr a -> Ptr a
unConstPtr' (ConstPtr p) = p

-- * Data types

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

data AbsInfo = AbsInfo
    { absValue :: Int32
    , absMinimum :: Int32
    , absMaximum :: Int32
    , absFuzz :: Int32
    , absFlat :: Int32
    , absResolution :: Int32
    }
    deriving (Show)

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

-- * Events

inputEventSize :: Int
inputEventSize = sizeOf (undefined :: Input_event)

nextEvent :: Device -> CUInt -> IO (Errno, CEvent)
nextEvent dev flags = withDevice dev $ \devPtr ->
    allocaBytes inputEventSize $ \evPtr -> do
        err <- Raw.libevdev_next_event devPtr flags (castPtr evPtr)
        ev <- getEvent evPtr
        pure (Errno err, ev)

nextEventMay :: Device -> CUInt -> IO (Errno, Maybe CEvent)
nextEventMay dev flags = withDevice dev $ \devPtr ->
    allocaBytes inputEventSize $ \evPtr -> do
        err <- Raw.libevdev_next_event devPtr flags (castPtr evPtr)
        if Errno err /= eOK
            then pure
                ( if negateErrno (Errno err) == eAGAIN then eOK else Errno err
                , Nothing
                )
            else do
                ev <- getEvent evPtr
                pure (eOK, Just ev)

getEvent :: Ptr Input_event -> IO CEvent
getEvent evPtr = do
    Input_event{time, type', code, value} <- peek evPtr
    let C__U16 (CUShort t) = type'
        C__U16 (CUShort c) = code
        C__S32 (CInt v) = value
        Timeval{tv_sec, tv_usec} = time
        C__Time_t (CLong sec) = tv_sec
        C__Suseconds_t (CLong usec) = tv_usec
    pure $ CEvent
        { cEventType = fromIntegral t
        , cEventCode = fromIntegral c
        , cEventValue = fromIntegral v
        , cEventTime = CTimeVal (fromIntegral sec) (fromIntegral usec)
        }

-- * Grabbing

grabDevice :: Device -> GrabMode -> IO Errno
grabDevice dev mode = withDevice dev $ \devPtr ->
    Errno <$> Raw.libevdev_grab devPtr (grabModeToRaw mode)

-- * Device properties (getters)

deviceFd :: Device -> IO Fd
deviceFd dev = withDevice dev $ \devPtr ->
    Fd <$> Raw.libevdev_get_fd (constPtr devPtr)

deviceName :: Device -> IO (IO ByteString)
deviceName dev = withDevice dev $ \devPtr -> do
    cstr <- Raw.libevdev_get_name (constPtr devPtr)
    pure $ packCString (unConstPtr' cstr)

devicePhys :: Device -> IO (IO (Maybe ByteString))
devicePhys dev = withDevice dev $ \devPtr -> do
    cstr <- Raw.libevdev_get_phys (constPtr devPtr)
    pure $ packCString' (unConstPtr' cstr)

deviceUniq :: Device -> IO (IO (Maybe ByteString))
deviceUniq dev = withDevice dev $ \devPtr -> do
    cstr <- Raw.libevdev_get_uniq (constPtr devPtr)
    pure $ packCString' (unConstPtr' cstr)

deviceProduct :: Device -> IO Int
deviceProduct dev = withDevice dev $ \devPtr ->
    fromIntegral <$> Raw.libevdev_get_id_product (constPtr devPtr)

deviceVendor :: Device -> IO Int
deviceVendor dev = withDevice dev $ \devPtr ->
    fromIntegral <$> Raw.libevdev_get_id_vendor (constPtr devPtr)

deviceBustype :: Device -> IO Int
deviceBustype dev = withDevice dev $ \devPtr ->
    fromIntegral <$> Raw.libevdev_get_id_bustype (constPtr devPtr)

deviceVersion :: Device -> IO Int
deviceVersion dev = withDevice dev $ \devPtr ->
    fromIntegral <$> Raw.libevdev_get_id_version (constPtr devPtr)

-- * Device properties (setters)

setDeviceName :: Device -> ByteString -> IO ()
setDeviceName dev name = withDevice dev $ \devPtr ->
    useAsCString name $ \cstr -> Raw.libevdev_set_name devPtr (constPtr cstr)

setDevicePhys :: Device -> ByteString -> IO ()
setDevicePhys dev phys = withDevice dev $ \devPtr ->
    useAsCString phys $ \cstr -> Raw.libevdev_set_phys devPtr (constPtr cstr)

setDeviceUniq :: Device -> ByteString -> IO ()
setDeviceUniq dev uniq = withDevice dev $ \devPtr ->
    useAsCString uniq $ \cstr -> Raw.libevdev_set_uniq devPtr (constPtr cstr)

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
    (/= 0) <$> Raw.libevdev_has_property (constPtr devPtr) (convertEnum prop)

hasEventType :: Device -> EventType -> IO Bool
hasEventType dev et = withDevice dev $ \devPtr ->
    (/= 0) <$> Raw.libevdev_has_event_type (constPtr devPtr) (convertEnum et)

hasEventCode :: Device -> Word16 -> Word16 -> IO Bool
hasEventCode dev t c = withDevice dev $ \devPtr ->
    (/= 0) <$> Raw.libevdev_has_event_code (constPtr devPtr) (fromIntegral t) (fromIntegral c)

-- * Abs info

getAbsInfo :: Device -> Word32 -> IO (Maybe AbsInfo)
getAbsInfo dev code = withDevice dev $ \devPtr -> do
    ptr <- Raw.libevdev_get_abs_info (constPtr devPtr) (CUInt code)
    let rawPtr = unConstPtr' ptr :: Ptr Input_absinfo
    if rawPtr == nullPtr
        then pure Nothing
        else do
            Input_absinfo
                { value      = C__S32 (CInt v)
                , minimum    = C__S32 (CInt mn)
                , maximum    = C__S32 (CInt mx)
                , fuzz       = C__S32 (CInt fz)
                , flat       = C__S32 (CInt fl)
                , resolution = C__S32 (CInt res)
                } <- peek rawPtr
            pure $ Just AbsInfo
                { absValue = v
                , absMinimum = mn
                , absMaximum = mx
                , absFuzz = fz
                , absFlat = fl
                , absResolution = res
                }

withAbsInfo :: AbsInfo -> (Ptr () -> IO a) -> IO a
withAbsInfo AbsInfo{..} f = do
    let info = Input_absinfo
            { value      = C__S32 (CInt absValue)
            , minimum    = C__S32 (CInt absMinimum)
            , maximum    = C__S32 (CInt absMaximum)
            , fuzz       = C__S32 (CInt absFuzz)
            , flat       = C__S32 (CInt absFlat)
            , resolution = C__S32 (CInt absResolution)
            }
    p <- mallocBytes (sizeOf info)
    poke (castPtr p) info
    fp <- newForeignPtr_ p
    withForeignPtr fp f

-- * Event enabling

enableType :: Device -> Word16 -> IO Errno
enableType dev t = withDevice dev $ \devPtr ->
    Errno <$> Raw.libevdev_enable_event_type devPtr (fromIntegral t)

enableCode :: Device -> Word16 -> Word16 -> Ptr () -> IO Errno
enableCode dev t c dataPtr = withDevice dev $ \devPtr ->
    Errno <$> Raw.libevdev_enable_event_code devPtr (fromIntegral t) (fromIntegral c) (constPtr $ castPtr dataPtr)

-- * Uinput

createFromDevice :: Device -> Fd -> IO (Errno, UDevice)
createFromDevice dev (Fd fd) = withDevice dev $ \devPtr -> do
    udevPtrPtr <- mallocForeignPtrBytes (sizeOf (undefined :: Ptr ()))
    (e, udevPtr) <- withForeignPtr udevPtrPtr $ \pp ->
        (,) <$> Raw.libevdev_uinput_create_from_device (constPtr devPtr) fd (castPtr pp) <*> peek pp
    udevFP <- newForeignPtr finalizer_libevdev_uinput_destroy udevPtr
    pure (Errno e, UDevice udevFP)

getSyspath :: UDevice -> IO (Maybe ByteString)
getSyspath dev = withUDevice dev $ \devPtr ->
    Raw.libevdev_uinput_get_syspath devPtr >>= packCString' . unConstPtr'

getDevnode :: UDevice -> IO (Maybe ByteString)
getDevnode dev = withUDevice dev $ \devPtr ->
    Raw.libevdev_uinput_get_devnode devPtr >>= packCString' . unConstPtr'

writeEvent :: UDevice -> Word16 -> Word16 -> Int32 -> IO Errno
writeEvent dev t c v = withUDevice dev $ \devPtr ->
    Errno <$> Raw.libevdev_uinput_write_event (constPtr devPtr) (fromIntegral t) (fromIntegral c) (fromIntegral v)

-- * LEDs

libevdev_kernel_set_led_value :: Device -> LEDEvent -> LEDValue -> IO Errno
libevdev_kernel_set_led_value dev led val = withDevice dev $ \devPtr ->
    Errno <$> Raw.libevdev_kernel_set_led_value devPtr (convertEnum led) (ledValueToRaw val)

-- * Util

convertEnum :: (Enum a, Integral b) => a -> b
convertEnum = fromIntegral . fromEnum

packCString' :: CString -> IO (Maybe ByteString)
packCString' p = if p == nullPtr then pure Nothing else Just <$> packCString p

negateErrno :: Errno -> Errno
negateErrno (Errno cint) = Errno (-cint)
