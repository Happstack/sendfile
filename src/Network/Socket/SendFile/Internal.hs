{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Network.Socket.SendFile.Internal (
    sendFile,
    sendFile',
    sendFileMode,
    unsafeSendFile,
    unsafeSendFile',
    ) where

import Prelude hiding (length)
import Control.Exception (finally)
import Data.Int (Int64)
import GHC.Handle (withHandle_)
import GHC.IOBase (haFD)
import Network.Socket (Socket(..), fdSocket)
import System.IO (
    Handle,
    IOMode(..),
    SeekMode(..),
    hFileSize,
    hFlush,
    hSeek,
    hTell,
    withBinaryFile
    )
import System.Posix.Types (Fd(..))

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
-- WARNING: mutated handle guarantee is not threadsafe for the portable implementation! (cannot portably duplicate a handle)
import Data.ByteString.Char8 (hGet, hPutStr, length)
import Network.Socket.ByteString (sendAll)

sendFileMode :: String
sendFileMode = "PORTABLE_SENDFILE"

sendFile' :: Socket -> Handle -> Integer -> Integer -> IO ()
sendFile' = wrapSendFile' $ \outs inp off count -> do
    befPos <- hTell inp
    finally
      (do hSeek inp AbsoluteSeek off
          rsend outs inp count)
      (hSeek inp AbsoluteSeek befPos)
    where rsend _    _   0        = return ()
          rsend outs inp reqBytes = do
              let bytes = min 4096 reqBytes :: Integer
              buf <- hGet inp (fromIntegral bytes)
              sendAll outs buf
              rsend outs inp (reqBytes - (fromIntegral $ length buf))

unsafeSendFile' :: Handle -> Handle -> Integer -> Integer -> IO ()
unsafeSendFile' = wrapSendFile' $ \outp inp off count -> do
    befPos <- hTell inp
    finally
      (do hSeek inp AbsoluteSeek off
          hPutStr outp =<< hGet inp (fromIntegral count))
      (hSeek inp AbsoluteSeek befPos)
    hFlush outp -- match the behavior that all data is "flushed to the os" of native implementations
#else
sendFile' :: Socket -> Handle -> Integer -> Integer -> IO ()
sendFile' outs inp off count =
    withHandle_ "Network.Socket.SendFile.sendFile'" inp $ \inp' -> do
    let out_fd = Fd (fdSocket outs)
    let in_fd = Fd (haFD inp')
    wrapSendFile' _sendFile out_fd in_fd off count

unsafeSendFile' :: Handle -> Handle -> Integer -> Integer -> IO ()
unsafeSendFile' outp inp off count = do
    hFlush outp -- flush outp before sending
    withHandle_ "Network.Socket.SendFile.unsafeSendFile'" outp $ \outp' -> do
    withHandle_ "Network.Socket.SendFile.unsafeSendFile'" inp $ \inp' -> do
    let out_fd = Fd (haFD outp')
    let in_fd = Fd (haFD inp')
    wrapSendFile' _sendFile out_fd in_fd off count
#endif

-- | wraps sendFile' to check arguments
wrapSendFile' :: Integral i => (a -> b -> i -> i -> IO ()) -> a -> b -> Integer -> Integer -> IO ()
wrapSendFile' fun outp inp off count
    | count < 0  = error "SendFile - count must be a positive integer"
    | count == 0 = return () -- Send nothing -- why do the work? Also, Windows treats '0' as 'send the whole file'.
    | otherwise  = fun outp inp (fromIntegral off) (fromIntegral count)

sendFile :: Socket -> FilePath -> IO ()
sendFile outs infp = withBinaryFile infp ReadMode $ \inp -> do
    count <- hFileSize inp
    sendFile' outs inp 0 (fromIntegral count)

unsafeSendFile :: Handle -> FilePath -> IO ()
unsafeSendFile outp infp = withBinaryFile infp ReadMode $ \inp -> do
    count <- hFileSize inp
    unsafeSendFile' outp inp 0 (fromIntegral count)

