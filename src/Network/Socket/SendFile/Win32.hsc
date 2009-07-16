{-# LANGUAGE ForeignFunctionInterface #-}
-- | Win32 system-dependent code for 'TransmitFile'.
module Network.Socket.SendFile.Win32 (_sendFile) where
import Foreign.C.Error (throwErrno)
import Foreign.Ptr (IntPtr, intPtrToPtr)
import Foreign.C.Types (CInt)
import System.Posix.Types (Fd)
import System.Win32.Types

#include <windows.h>

_sendFile :: Fd -> Fd -> Integer -> IO ()
_sendFile out_fd in_fd count = do
    in_hdl <- get_osfhandle in_fd
    transmitFile (fromIntegral out_fd) in_hdl (fromIntegral count)

get_osfhandle :: Fd        -- ^ User file descriptor.
              -> IO HANDLE -- ^ The operating-system file handle.
get_osfhandle fd = do
    res <- c_get_osfhandle fd
    if res == (#const INVALID_HANDLE_VALUE)
      then throwErrno "Network.Socket.SendFile.Win32.get_osfhandle"
      else return (intPtrToPtr res)

transmitFile :: Fd     -- ^ A handle to a connected socket.
             -> HANDLE -- ^ A handle to the open file that the TransmitFile function transmits.
             -> DWORD  -- ^ The number of bytes in the file to transmit.
             -> IO ()
transmitFile out_fd in_hdl count =
    failIf_ (== 0) "Network.Socket.SendFile.Win32.transmitFile" (c_TransmitFile out_fd in_hdl count)
    
foreign import ccall unsafe
    c_get_osfhandle :: Fd -> IO IntPtr
    
foreign import ccall unsafe
    c_TransmitFile :: Fd -> HANDLE -> DWORD -> IO CInt
