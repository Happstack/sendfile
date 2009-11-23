{-# LANGUAGE CPP #-}
module Network.Socket.SendFile.Internal (
    sendFile,
    sendFile',
    sendFileMode,
    ) where
#if defined(PORTABLE_SENDFILE)
import Data.ByteString.Char8 (hGet, hPutStr, length, ByteString)
import Network.Socket.ByteString (sendAll)
import Network.Socket (Socket(..))
import Prelude hiding (length)
import System.IO (Handle, IOMode(..), SeekMode(..), hFileSize, hSeek, withBinaryFile)
#else
import Network.Socket (Socket(..), fdSocket)
import System.IO (Handle, IOMode(..), hFileSize, withBinaryFile)
import System.Posix.Types (Fd(..))
#endif
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ >= 611
import GHC.IO.Handle
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Types
import qualified GHC.IO.FD as FD
import qualified GHC.IO.Handle.FD as FD
import GHC.IO.Exception
import Data.Typeable (cast)
#else
import GHC.IOBase
import GHC.Handle hiding (fdToHandle)
import qualified GHC.Handle
#endif
#endif

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

#if defined(DARWIN_SENDFILE)
import Network.Socket.SendFile.Darwin (_sendFile)

sendFileMode :: String
sendFileMode = "DARWIN_SENDFILE"
#endif

#if defined(PORTABLE_SENDFILE)
sendFileMode :: String
sendFileMode = "PORTABLE_SENDFILE"

sendFile'' :: Socket -> Handle -> Integer -> Integer -> IO ()
sendFile' = wrapSendFile' $ \outs inp off count -> do
    hSeek inp AbsoluteSeek off
    rsend (sendAll outs) inp count

rsend :: (ByteString -> IO ()) -> Handle -> Integer -> IO ()
rsend write inp n = do
  loop n
  where
    loop 0        = return ()
    loop reqBytes = do
      let bytes = min 32768 reqBytes :: Integer
      buf <- hGet inp (fromIntegral bytes)
      write buf
      loop $ reqBytes - (fromIntegral $ length buf)
#else
sendFile'' :: Socket -> Handle -> Integer -> Integer -> IO ()
sendFile'' outs inp off count =
    do let out_fd = Fd (fdSocket outs)
       in_fd <- handleToFd inp
       wrapSendFile' _sendFile out_fd in_fd off count
#endif

sendFile :: Socket -> FilePath -> IO ()
sendFile outs infp = withBinaryFile infp ReadMode $ \inp -> do
    count <- hFileSize inp
    sendFile'' outs inp 0 count

sendFile' :: Socket -> FilePath -> Integer -> Integer -> IO ()
sendFile' outs infp offset count =
    withBinaryFile infp ReadMode $ \inp ->
        sendFile'' outs inp offset count
          
-- | wraps sendFile' to check arguments
wrapSendFile' :: Integral i => (a -> b -> i -> i -> IO ()) -> a -> b -> Integer -> Integer -> IO ()
wrapSendFile' fun outp inp off count
    | off < 0    = error "SendFile - offset must be a positive integer"
    | count < 0  = error "SendFile - count must be a positive integer"
    | count == 0 = return () -- Send nothing -- why do the work? Also, Windows treats '0' as 'send the whole file'.
    | otherwise  = fun outp inp (fromIntegral off) (fromIntegral count)

handleToFd :: Handle -> IO Fd
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ >= 611
handleToFd h = withHandle "handleToFd" h $ \ h_@Handle__{haType=_,..} -> do
  case cast haDevice of
    Nothing -> ioError (ioeSetErrorString (mkIOError IllegalOperation
                                           "handleToFd" (Just h) Nothing) 
                        "handle is not a file descriptor")
    Just fd -> do
     -- converting a Handle into an Fd effectively means
     -- letting go of the Handle; it is put into a closed
     -- state as a result. 
     flushWriteBuffer h_
     FD.release fd
     return (Handle__{haType=ClosedHandle,..}, Fd (fromIntegral (FD.fdFD fd)))
#else
handleToFd h = withHandle "handleToFd" h $ \ h_ -> do
  -- converting a Handle into an Fd effectively means
  -- letting go of the Handle; it is put into a closed
  -- state as a result. 
  let fd = haFD h_
  flushWriteBufferOnly h_
  unlockFile (fromIntegral fd)
    -- setting the Handle's fd to (-1) as well as its 'type'
    -- to closed, is enough to disable the finalizer that
    -- eventually is run on the Handle.
  return (h_{haFD= (-1),haType=ClosedHandle}, Fd (fromIntegral fd))
#endif
#endif
