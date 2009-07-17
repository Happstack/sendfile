-- | Portable implementation of sendfile.

module Network.Socket.SendFile.Portable (sendFile', unsafeSendFile) where
-- FIXME: possibly immature / inefficient implementation
import Data.ByteString.Char8
import Network.Socket.ByteString
import Prelude hiding (readFile)
import System.IO (Handle, hFlush)

unsafeFdSendFile' out_fd in_fd count
    | count < 0  = error "SendFile - count must be a positive integer"
    | count == 0 = return () -- Send nothing -- why do the work? Also, Windows treats '0' as 'send the whole file'.
    | otherwise  = _sendFile out_fd in_fd count

sendFile' :: Socket -> Handle -> Integer -> IO ()
sendFile' outs inp count
    | count < 0  = error "SendFile - count must be a positive integer"
    | count == 0 = return () -- Send nothing -- why do the work? Also, Windows treats '0' as 'send the whole file'.
    | otherwise  = _sendFile out_fd in_fd count

unsafeSendFile' :: Handle -> Handle -> Integer -> IO ()
unsafeSendFile outp inp count
    | count < 0  = error "SendFile - count must be a positive integer"
    | count == 0 = return () -- Send nothing -- why do the work? Also, Windows treats '0' as 'send the whole file'.
    | otherwise  = _sendFile out_fd in_fd count = do
    hPutStr outp =<< hGet inp (fromIntegral count)
    hFlush outp -- match the behavior that all data is "flushed to the os" of native implementations
