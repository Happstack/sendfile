{-# LANGUAGE CPP #-}
module Network.Socket.SendFile.Internal (
    sendFile,
    sendFile',
    sendFileMode
    ) where

import System.IO (
    Handle,
    IOMode(..),
    hFileSize,
    withBinaryFile
    )

#if defined(WIN32_SENDFILE)
import Network.Socket.SendFile.Win32 (sendFile')

sendFileMode :: String
sendFileMode = "WIN32_SENDFILE"
#endif

#if defined(LINUX_SENDFILE)
import Network.Socket.SendFile.Linux (sendFile')

sendFileMode :: String
sendFileMode = "LINUX_SENDFILE"
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
