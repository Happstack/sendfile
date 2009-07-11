{-# LANGUAGE ForeignFunctionInterface #-}
-- | FreeBSD system-dependent code for 'sendfile'.
module Network.Socket.SendFile.FreeBSD (_sendFile) where
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types (CInt, CSize)
import Foreign.Ptr (Ptr, nullPtr)
import GHC.IOBase (FD, haFD)
import GHC.Handle (withHandle_)
import System.IO (Handle)
import System.Posix.Types (COff)

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>

_sendFile :: Handle -> Handle -> Integer -> IO ()
_sendFile outp inp count = do
  withHandle_ "Network.Socket.SendFile.FreeBSD.sendFile'" outp $ \outp' ->
    withHandle_ "Network.Socket.SendFile.FreeBSD.sendFile'" inp $ \inp' -> do
    let out_fd = haFD outp'
    let in_fd = haFD inp'
    throwErrnoIfMinus1 "Network.Socket.SendFile.FreeBSD.sendFile'" $
      c_sendfile_freebsd in_fd out_fd 0 (fromIntegral count) nullPtr nullPtr 0
    return ()

foreign import ccall unsafe "sendfile" c_sendfile_freebsd
  :: FD -> FD -> COff -> CSize -> Ptr () -> Ptr COff -> CInt -> IO CInt
