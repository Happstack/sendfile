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
import Network.Socket.SendFile.Win32 (_sendFile)

sendFileMode :: String
sendFileMode = "WIN32_SENDFILE"
#endif

#if defined(LINUX_SENDFILE)
import Network.Socket.SendFile.Linux (_sendFile)

sendFileMode :: String
sendFileMode = "LINUX_SENDFILE"
#endif

#if defined(FREEBSD_SENDFILE)
import Network.Socket.SendFile.FreeBSD (_sendFile)

sendFileMode :: String
sendFileMode = "FREEBSD_SENDFILE"
#endif

#if defined(PORTABLE_SENDFILE)
import Network.Socket.SendFile.Portable (_sendFile)

sendFileMode :: String
sendFileMode = "PORTABLE_SENDFILE"
#endif

sendFile :: Handle -> FilePath -> IO ()
sendFile outp infp = withBinaryFile infp ReadMode $ \inp -> do
    count <- hFileSize inp
    sendFile' outp inp count

sendFile' :: Handle -> Handle -> Integer -> IO ()
sendFile' outp inp count
    | count < 0  = error "sendFile' - count must be a positive integer"
    | count == 0 = return () -- Send nothing -- why do the work? Also, Windows treats '0' as 'send the whole file'.
    | otherwise  = _sendFile outp inp count
