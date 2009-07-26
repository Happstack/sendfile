{-# LANGUAGE ForeignFunctionInterface #-}
-- | Linux system-dependent code for 'sendfile'.
module Network.Socket.SendFile.Linux (_sendFile) where
import Data.Int
import Data.Word
import Foreign.C.Error (eAGAIN, getErrno, throwErrno)
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable(peek, poke)
import System.Posix.Types (Fd)

#include <sys/sendfile.h>

_sendFile :: Fd -> Fd -> Int64 -> Int64 -> IO ()
_sendFile out_fd in_fd off count = do
    alloca $ \poff -> do
    poke poff (fromIntegral off)
    rsendfile out_fd in_fd poff count

rsendfile :: Fd -> Fd -> Ptr (#type off_t) -> Int64 -> IO ()
rsendfile out_fd in_fd poff 0         = return ()
rsendfile out_fd in_fd poff remaining = do
    let top = fromIntegral (maxBound :: (#type ssize_t)) :: Int64
        bytes = fromIntegral (min remaining top) :: (#type size_t)
    sbytes <- fmap fromIntegral (sendfile out_fd in_fd poff (fromIntegral bytes))
    rsendfile out_fd in_fd poff (remaining - (fromIntegral sbytes))
      
    
sendfile :: Fd -> Fd -> Ptr (#type off_t) -> (#type size_t) -> IO (#type ssize_t)
sendfile out_fd in_fd poff bytes = do
    sbytes <- c_sendfile out_fd in_fd poff bytes
    if sbytes <= -1
      then do
        errno <- getErrno
        case errno of
            eAGAIN -> sendfile out_fd in_fd poff bytes
            _      -> throwErrno "Network.Socket.SendFile.Linux"
      else
        return sbytes

-- sendfile64 gives LFS support
foreign import ccall unsafe "sendfile64" c_sendfile
    :: Fd -> Fd -> Ptr (#type off_t) -> (#type size_t) -> IO (#type ssize_t)

