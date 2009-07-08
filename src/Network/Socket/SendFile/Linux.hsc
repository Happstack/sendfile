{-# LANGUAGE ForeignFunctionInterface #-}
-- | Linux system-dependent code for 'sendfile'.
module Network.Socket.SendFile.Linux (sendFile') where
import Data.Int
import Data.Word
import Foreign.C.Error (throwErrno)
import GHC.IOBase (FD, haFD)
import GHC.Handle (withHandle_)
import System.IO (Handle, hFlush)

#include <sys/sendfile.h>

sendFile' :: Handle -> Handle -> Integer -> IO ()
sendFile' outp inp count = do
    -- flush outp before sending
    hFlush outp
    withHandle_ "Network.Socket.SendFile.Linux.sendFile'" outp $ \outp' -> do
    withHandle_ "Network.Socket.SendFile.Linux.sendFile'" inp $ \inp' -> do 
    let out_fd = haFD outp'
    let in_fd = haFD inp'
    res <- c_sendfile_linux out_fd in_fd (fromIntegral count)
    if res == -1
        then throwErrno "Network.Socket.SendFile.Linux.sendFile'"
        else return ()

foreign import ccall
    c_sendfile_linux :: FD -> FD -> (#type size_t) -> IO (#type ssize_t)
