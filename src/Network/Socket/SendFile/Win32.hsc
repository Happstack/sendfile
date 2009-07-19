{-# LANGUAGE ForeignFunctionInterface #-}
-- | Win32 system-dependent code for 'TransmitFile'.
module Network.Socket.SendFile.Win32 (_sendFile) where
import Foreign.C.Error (throwErrnoIf)
import Foreign.Ptr (IntPtr, Ptr, intPtrToPtr, nullPtr)
import Foreign.C.Types (CInt)
import System.Posix.Types (Fd)
import System.Win32.Types (DWORD, HANDLE, failIfZero)

#include <windows.h>
#include <mswsock.h>

type SOCKET = Fd

_sendFile :: Fd -> Fd -> Integer -> IO Integer
_sendFile out_fd in_fd count = do
    in_hdl <- get_osfhandle in_fd
    transmitFile out_fd in_hdl (fromIntegral count)
    -- according to msdn:
    --   If the TransmitFile function is called with the lpOverlapped parameter
    --   set to NULL, the operation is executed as synchronous I/O. The function
    --   will not complete until the file has been sent.
    return count

get_osfhandle :: Fd        -- ^ User file descriptor.
              -> IO HANDLE -- ^ The operating-system file handle.
get_osfhandle fd = do
    res <- throwErrnoIf
             (== (#const INVALID_HANDLE_VALUE))
             "Network.Socket.SendFile.Win32.get_osfhandle"
             (c_get_osfhandle fd)
    return (intPtrToPtr res)

transmitFile :: SOCKET -- ^ A handle to a connected socket.
             -> HANDLE -- ^ A handle to the open file that the TransmitFile function transmits.
             -> DWORD  -- ^ The number of bytes in the file to transmit.
             -> IO ()
transmitFile out_fd in_hdl count = do
    failIfZero
      "Network.Socket.SendFile.Win32.transmitFile"
      (c_TransmitFile out_fd in_hdl count 0 nullPtr nullPtr (#const TF_USE_KERNEL_APC))
    return ()

-- http://support.microsoft.com/kb/99173 - MAY BE IMPORTANT
-- http://msdn.microsoft.com/en-us/library/ks2530z6.aspx
foreign import ccall unsafe "io.h _get_osfhandle"
    c_get_osfhandle :: Fd
                    -> IO IntPtr

-- http://msdn.microsoft.com/en-us/library/ms740565(VS.85).aspx
foreign import stdcall unsafe "mswsock.h TransmitFile"
    c_TransmitFile :: SOCKET
                   -> HANDLE
                   -> DWORD
                   -> DWORD
                   -> Ptr ()
                   -> Ptr ()
                   -> DWORD
                   -> IO CInt

