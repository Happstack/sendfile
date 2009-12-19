{-# LANGUAGE ForeignFunctionInterface #-}
-- | FreeBSD system-dependent code for 'sendfile'.
module Network.Socket.SendFile.FreeBSD (_sendFile) where
import Control.Concurrent (threadWaitWrite)
import Foreign.C.Error (eAGAIN, eINTR, getErrno, throwErrno)
import Foreign.C.Types (CInt, CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import System.Posix.Types (COff, Fd)

_sendFile :: Fd -> Fd -> Integer -> Integer -> IO ()
_sendFile out_fd in_fd off count = 
    alloca $ \sbytes -> do
      rsendfile out_fd in_fd (fromIntegral off) (fromIntegral count) sbytes

rsendfile :: Fd -> Fd -> COff -> CSize -> Ptr COff -> IO ()
rsendfile _ _ _ 0 _ = return ()
rsendfile out_fd in_fd off remaining sbytes =
    do let bytes = min remaining maxBytes
       sent <- sendfile out_fd in_fd off bytes sbytes 0
       rsendfile out_fd in_fd (off + sent) (remaining `safeMinus` (fromIntegral  sent)) sbytes

sendfile :: Fd -> Fd -> COff -> CSize -> Ptr COff -> COff -> IO COff
sendfile out_fd in_fd off count sbytes totalSent =
    do threadWaitWrite out_fd
       res <- c_sendfile_freebsd in_fd out_fd off count nullPtr sbytes 0
       if (res == 0)
          then do nbytes <- peek sbytes
                  return (totalSent + nbytes)
          else do errno <- getErrno
                  if (errno == eAGAIN) || (errno == eINTR) 
                   then do nbytes <- peek sbytes
                           sendfile out_fd in_fd (off + nbytes) (count `safeMinus` (fromIntegral nbytes)) sbytes (totalSent + nbytes)
                   else throwErrno "Network.Socket.SendFile.FreeBSD.sendfile"

safeMinus :: (Ord a, Num a) => a -> a -> a
safeMinus x y
    | y >= x = 0
    | otherwise = x - y

-- max num of bytes in one send
maxBytes :: CSize
maxBytes = maxBound :: CSize

foreign import ccall unsafe "sys/uio.h sendfile" c_sendfile_freebsd
    :: Fd -> Fd -> COff -> CSize -> Ptr () -> Ptr COff -> CInt -> IO CInt
