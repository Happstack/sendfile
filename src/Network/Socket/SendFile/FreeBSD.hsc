{-# LANGUAGE ForeignFunctionInterface #-}
-- | FreeBSD system-dependent code for 'sendfile'.
module Network.Socket.SendFile.FreeBSD (_sendFile) where
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types (CInt, CSize)
import Foreign.Ptr (Ptr, nullPtr)
import System.IO (Handle)
import System.Posix.Types (COff, Fd)

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>

_sendFile :: Fd -> Fd -> Integer -> IO ()
_sendFile out_fd in_fd count = do
    throwErrnoIfMinus1 "Network.Socket.SendFile.FreeBSD.sendFile'" $
      c_sendfile_freebsd in_fd out_fd 0 (fromIntegral count) nullPtr nullPtr 0
    return ()

foreign import ccall unsafe "sendfile" c_sendfile_freebsd
    :: Fd -> Fd -> COff -> CSize -> Ptr () -> Ptr COff -> CInt -> IO CInt
