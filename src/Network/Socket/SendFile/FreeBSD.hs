{-# LANGUAGE ForeignFunctionInterface #-}
-- | FreeBSD system-dependent code for 'sendfile'.
module Network.Socket.SendFile.FreeBSD (_sendFile) where
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types (CInt, CSize)
import Foreign.Ptr (Ptr, nullPtr)
import System.Posix.Types (COff, Fd)

_sendFile :: Fd -> Fd -> Integer -> IO Integer
_sendFile out_fd in_fd count =
    fmap fromIntegral $ throwErrnoIfMinus1
      "Network.Socket.SendFile.FreeBSD.sendFile'"
      (c_sendfile_freebsd in_fd out_fd 0 (fromIntegral count) nullPtr nullPtr 0)

foreign import ccall unsafe "sys/uio.h sendfile" c_sendfile_freebsd
    :: Fd -> Fd -> COff -> CSize -> Ptr () -> Ptr COff -> CInt -> IO CInt

