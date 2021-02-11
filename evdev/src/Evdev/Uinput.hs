module Evdev.Uinput (
    LL.UDevice,
    newUDevice,
    NewUDevice (..),
    defaultNewUDevice,
    LL.AbsInfo (..),
    writeEvent,
    writeBatch,
) where

import Control.Monad
import Data.Tuple.Extra
import Foreign

import Data.ByteString.Char8 (ByteString)

import Evdev
import Evdev.Codes
import qualified Evdev.LowLevel as LL
import Util

defaultNewUDevice :: ByteString -> NewUDevice
defaultNewUDevice name =
    NewUDevice
        { name
        , uniq = Nothing
        , phys = Nothing
        , idProduct = Nothing
        , idVendor = Nothing
        , idBustype = Nothing
        , idVersion = Nothing
        , keys = []
        , relAxes = []
        , absAxes = []
        , miscs = []
        , switchs = []
        , leds = []
        , sounds = []
        , reps = []
        , ffs = []
        , powers = []
        , ffStats = []
        }
data NewUDevice = NewUDevice
    { name :: ByteString
    , phys :: Maybe ByteString
    , uniq :: Maybe ByteString
    , idProduct :: Maybe Int
    , idVendor :: Maybe Int
    , idBustype :: Maybe Int
    , idVersion :: Maybe Int
    , keys :: [Key]
    , relAxes :: [RelativeAxis]
    , absAxes :: [(AbsoluteAxis, LL.AbsInfo)]
    , miscs :: [MiscEvent]
    , switchs :: [SwitchEvent]
    , leds :: [LEDEvent]
    , sounds :: [SoundEvent]
    , reps :: [(RepeatEvent, Int)]
    , ffs :: [EventCode]
    , powers :: [EventCode]
    , ffStats :: [EventCode]
    }
newUDevice :: NewUDevice -> IO LL.UDevice
newUDevice NewUDevice{..} = do
    dev <- LL.libevdev_new
    LL.setDeviceName dev name

    let maybeSet :: (LL.Device -> a -> IO ()) -> Maybe a -> IO ()
        maybeSet setter x = maybe (pure ()) (setter dev) x
    maybeSet LL.setDevicePhys phys
    maybeSet LL.setDeviceUniq uniq
    maybeSet LL.libevdev_set_id_product idProduct
    maybeSet LL.libevdev_set_id_vendor idVendor
    maybeSet LL.libevdev_set_id_bustype idBustype
    maybeSet LL.libevdev_set_id_version idVersion

    let enable :: Ptr () -> EventType -> [Word16] -> IO ()
        enable ptr t cs = do
            unless (null cs) $ cec $ LL.enableType dev t'
            forM_ cs $ \c -> cec $ LL.enableCode dev t' c ptr
          where
            t' = fromEnum' t

    mapM_
        (uncurry $ enable nullPtr)
        [ (EvKey, map fromEnum' keys)
        , (EvRel, map fromEnum' relAxes)
        , (EvMsc, map fromEnum' miscs)
        , (EvSw, map fromEnum' switchs)
        , (EvLed, map fromEnum' leds)
        , (EvSnd, map fromEnum' sounds)
        , (EvFf, map fromEnum' ffs)
        , (EvPwr, map fromEnum' powers)
        , (EvFfStatus, map fromEnum' ffStats)
        ]

    forM_ reps $ \(rep, n) -> do
        pf <- mallocForeignPtr
        withForeignPtr pf \p -> do
            poke p n
            enable (castPtr p) EvRep [fromEnum' rep]

    forM_ absAxes $ \(axis, absInfo) ->
        LL.withAbsInfo absInfo $ \ptr ->
            enable ptr EvAbs [fromEnum' axis]

    cec $ LL.createFromDevice dev $ fromEnum' LL.UOMManaged
  where
    cec :: CErrCall a => IO a -> IO (CErrCallRes a)
    cec = cErrCall "newUDevice" ()

-- | Write a single event. Doesn't issue a sync event, so `writeEvent dev e /= writeBatch dev [e]`.
writeEvent :: LL.UDevice -> EventData -> IO ()
writeEvent dev e = do
    cErrCall "writeEvent" dev $ uncurry3 (LL.writeEvent dev) $ toCEventData e

-- | Write several events followed by a 'SynReport'.
writeBatch :: Foldable t => LL.UDevice -> t EventData -> IO ()
writeBatch dev es = do
    forM_ es $ writeEvent dev
    writeEvent dev $ SyncEvent SynReport
