{-# LANGUAGE ForeignFunctionInterface #-}
-- | FreeBSD system-dependent code for 'sendfile'.
module Network.Socket.SendFile.FreeBSD (_sendFile) where
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt, CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import System.Posix.Types (COff, Fd)

_sendFile :: Fd -> Fd -> Integer -> IO Integer
_sendFile out_fd in_fd count = alloca $ \sbytes -> do
    throwErrnoIfMinus1_
      "Network.Socket.SendFile.FreeBSD.sendFile'"
      (c_sendfile_freebsd in_fd out_fd 0 (fromIntegral count) nullPtr sbytes 0)
    fmap fromIntegral (peek sbytes)

foreign import ccall unsafe "sys/uio.h sendfile" c_sendfile_freebsd
    :: Fd -> Fd -> COff -> CSize -> Ptr () -> Ptr COff -> CInt -> IO CInt

