-- | Portable implementation of sendfile.
module Network.Socket.SendFile.Portable (_sendFile) where

import Data.ByteString.Char8
import Prelude hiding (readFile)
import System.IO (Handle, hFlush)


-- FIXME: possibly immature / inefficient implementation
_sendFile :: Handle -> Handle -> Integer -> IO ()
_sendFile outp inp count = do
    hPutStr outp =<< hGet inp (fromIntegral count)
    hFlush outp -- match the behavior that all data is "flushed to the os" of native implementations
