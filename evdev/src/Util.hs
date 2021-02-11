module Util where

import qualified Data.ByteString.Char8 as BS
import Foreign.C.Error (Errno (Errno), errnoToIOError)
import System.Posix.ByteString (RawFilePath)

import qualified Evdev.LowLevel as LL

fromEnum' :: (Num c, Enum a) => a -> c
fromEnum' = fromIntegral . fromEnum

--TODO careful - for some C calls (eg. libevdev_enable_event_code),
-- int returned doesn't necessarily correspond to a particular error number
--TODO this kinda seems like overkill, but things were getting ugly without it...
class CErrInfo a where
    cErrInfo :: a -> IO (Maybe RawFilePath)
instance CErrInfo () where
    cErrInfo () = return Nothing
instance CErrInfo RawFilePath where
    cErrInfo = pure . pure
instance CErrInfo LL.UDevice where
    cErrInfo = LL.getSyspath

-- for c actions which return an error value (0 for success)
-- run the action, throwing a relevant exception if the C errno is not 0
class CErrCall a where
    type CErrCallRes a
    cErrCall :: CErrInfo info => String -> info -> IO a -> IO (CErrCallRes a)
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
                path' <- cErrInfo info
                ioError $ errnoToIOError func (Errno $ abs n) Nothing $ BS.unpack <$> path'
