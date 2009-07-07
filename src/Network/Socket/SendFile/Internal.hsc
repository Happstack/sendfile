{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Network.Socket.SendFile.Internal (
    sendFile,
    sendFile',
    sendFileMode
    ) where

import System.IO (
    Handle,
    IOMode(..),
    hFileSize,
    hFlush,
    withBinaryFile
    )

#if defined(WIN32_SENDFILE)
import Network.Socket.SendFile.Win32 (sendFile')

sendFileMode :: String
sendFileMode = "WIN32_SENDFILE"
#endif

#if defined(LINUX_SENDFILE)
import Foreign.C
import GHC.IOBase (haFD)
import GHC.Handle (withHandle_)

sendFileMode :: String
sendFileMode = "LINUX_SENDFILE"

sendFile' :: Handle -> FilePath -> Int -> IO ()
sendFile' outh infp offset count = do
    -- flush outh before handing it sendFile
    hFlush outh
    withHandle_ "sendFile" outh $ \outh' -> do 
    withCString infp $ \in_fp -> do
    let out_fd = haFD outh'
    err <- c_sendfile_linux out_fd in_fp (fromIntegral offset) (fromIntegral count)
    if err == 0
        then return ()
        else fail ("errno " ++ show err)

foreign import ccall
    c_sendfile_linux :: CInt -> CString -> CLong -> CLong -> IO Int
#endif

#if defined(PORTABLE_SENDFILE)
import Network.Socket.SendFile.Portable (sendFile')

sendFileMode :: String
sendFileMode = "PORTABLE_SENDFILE"
#endif

sendFile :: Handle -> FilePath -> IO ()
sendFile outp infp = withBinaryFile infp ReadMode $ \inp -> do
    count <- hFileSize inp
    sendFile' outp inp count
