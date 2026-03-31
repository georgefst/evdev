module Util where

import Data.ByteString (ByteString, packCString)
import qualified Data.ByteString.Char8 as BS
import Data.Tuple (swap)
import Foreign (Ptr, nullPtr)
import Foreign.C (CString)
import Foreign.C.Error (Errno (Errno), errnoToIOError)
import System.Posix.ByteString (RawFilePath)

fromEnum' :: (Num c, Enum a) => a -> c
fromEnum' = fromIntegral . fromEnum

handleNull :: b -> (Ptr a -> b) -> Ptr a -> b
handleNull def f p = if p == nullPtr then def else f p

packCString' :: CString -> IO (Maybe ByteString)
packCString' = handleNull (return Nothing) (fmap Just . packCString)

toBool :: (Eq a, Num a) => a -> Bool
toBool = (/= 0)

--TODO careful - for some C calls (eg. libevdev_enable_event_code),
-- int returned doesn't necessarily correspond to a particular error number
--TODO this kinda seems like overkill, but things were getting ugly without it...
-- for c actions which return an error value (0 for success)
-- run the action, throwing a relevant exception if the C errno is not 0
class CErrCall a where
    type CErrCallRes a
    cErrCall :: String -> IO (Maybe RawFilePath) -> IO a -> IO (CErrCallRes a)
instance CErrCall Errno where
    type CErrCallRes Errno = ()
    cErrCall func path x = cErrCall func path $ (,()) <$> x
instance CErrCall (Errno, a) where
    type CErrCallRes (Errno, a) = a
    cErrCall func info x = do
        (errno, res) <- x
        case errno of
            Errno 0 -> return res
            Errno n -> do
                path' <- info
                ioError $ errnoToIOError func (Errno $ abs n) Nothing $ BS.unpack <$> path'
instance CErrCall (IO a, Errno) where
    type CErrCallRes (IO a, Errno) = a
    cErrCall func info x = cErrCall @(Errno, a) func info $ sequence =<< swap <$> x
