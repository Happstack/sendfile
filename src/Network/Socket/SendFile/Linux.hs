{-# LANGUAGE ForeignFunctionInterface #-}
-- | Linux system-dependent code for 'sendfile'.
module Network.Socket.SendFile.Linux (_sendFile) where
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CSize)
import Foreign.Ptr (Ptr, nullPtr)
import System.Posix.Types (COff, Fd)

_sendFile :: Fd -> Fd -> Integer -> IO ()
_sendFile out_fd in_fd count =
    throwErrnoIfMinus1_
      "Network.Socket.SendFile.Linux.sendFile'"
      (c_sendfile_linux out_fd in_fd nullPtr (fromIntegral count))

foreign import ccall unsafe "sys/sendfile.h sendfile" c_sendfile_linux
    :: Fd -> Fd -> Ptr COff -> CSize -> IO CSize

