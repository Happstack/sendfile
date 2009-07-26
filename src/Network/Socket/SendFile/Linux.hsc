{-# LANGUAGE ForeignFunctionInterface #-}
-- | Linux system-dependent code for 'sendfile'.
module Network.Socket.SendFile.Linux (_sendFile) where
import Data.Int
import Data.Word
import Foreign.C.Error (eAGAIN, getErrno, throwErrno)
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable(poke)
import System.Posix.Types (Fd)

#include <sys/sendfile.h>

_sendFile :: Fd -> Fd -> Int64 -> Int64 -> IO ()
_sendFile out_fd in_fd off count = do
    alloca $ \poff -> do
    poke poff off
    rsendfile out_fd in_fd poff count

rsendfile :: Fd -> Fd -> Ptr Int64 -> Int64 -> IO ()
rsendfile _      _     _    0         = return ()
rsendfile out_fd in_fd poff remaining = do
    let bytes = min remaining maxBytes
    sbytes <- sendfile out_fd in_fd poff bytes
    rsendfile out_fd in_fd poff (remaining - sbytes)
    
sendfile :: Fd -> Fd -> Ptr Int64 -> Int64 -> IO Int64
sendfile out_fd in_fd poff bytes = do
    sbytes <- c_sendfile out_fd in_fd poff (fromIntegral bytes)
    if sbytes <= -1
      then do errno <- getErrno
              if errno == eAGAIN
                then sendfile out_fd in_fd poff bytes
                else throwErrno "Network.Socket.SendFile.Linux"
      else return (fromIntegral sbytes)

-- max num of bytes in one send
maxBytes :: Int64
maxBytes = fromIntegral (maxBound :: (#type ssize_t))

-- sendfile64 gives LFS support
foreign import ccall unsafe "sendfile64" c_sendfile
    :: Fd -> Fd -> Ptr (#type off_t) -> (#type size_t) -> IO (#type ssize_t)

