{-# LANGUAGE CPP #-}
module Network.Socket.SendFile.Internal (
    sendFile,
    sendFile',
    sendFileMode,
    unsafeFdSendFile',
    unsafeSendFile,
    unsafeSendFile',
    ) where

import Network.Socket (Socket(..), fdSocket)
import System.IO (
    Handle,
    IOMode(..),
    hFileSize,
    hFlush,
    withBinaryFile
    )
import System.Posix.Types (Fd(..))
import GHC.Handle (withHandle_)
import GHC.IOBase (haFD)

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

sendFile :: Socket -> FilePath -> IO ()
sendFile outs infp = withBinaryFile infp ReadMode $ \inp -> do
    count <- hFileSize inp
    sendFile' outs inp count

sendFile' :: Socket -> Handle -> Integer -> IO ()
sendFile' outs inp count =
    withHandle_ "Network.Socket.SendFile.sendFile'" inp $ \inp' -> do
    let out_fd = Fd (fdSocket outs)
    let in_fd = Fd (haFD inp')
    unsafeFdSendFile' out_fd in_fd count

unsafeSendFile :: Handle -> FilePath -> IO ()
unsafeSendFile outp infp = withBinaryFile infp ReadMode $ \inp -> do
    count <- hFileSize inp
    unsafeSendFile' outp inp count

unsafeSendFile' :: Handle -> Handle -> Integer -> IO ()
unsafeSendFile' outp inp count = do
    hFlush outp -- flush outp before sending
    withHandle_ "Network.Socket.SendFile.unsafeSendFile'" outp $ \outp' -> do
    withHandle_ "Network.Socket.SendFile.unsafeSendFile'" inp $ \inp' -> do
    let out_fd = Fd (haFD outp')
    let in_fd = Fd (haFD inp')
    unsafeFdSendFile' out_fd in_fd count

unsafeFdSendFile' :: Fd -> Fd -> Integer -> IO ()
unsafeFdSendFile' out_fd in_fd count
    | count < 0  = error "SendFile - count must be a positive integer"
    | count == 0 = return () -- Send nothing -- why do the work? Also, Windows treats '0' as 'send the whole file'.
    | otherwise  = _sendFile out_fd in_fd count
