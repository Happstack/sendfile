{-# LANGUAGE ForeignFunctionInterface #-}
-- | Win32 system-dependent code for 'TransmitFile'.
module Network.Socket.SendFile.Win32 (_sendFile) where
import Data.Bits ((.|.))
import Data.Int
import Foreign.C.Error (throwErrnoIf)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (IntPtr, Ptr, intPtrToPtr, nullPtr)
import Foreign.Storable (peek)
import System.Posix.Types (Fd)
import System.Win32.Types (DWORD, HANDLE, failIfZero)

#include <windows.h>
#include <mswsock.h>

type SOCKET = Fd

_sendFile :: Fd -> Fd -> Int64 -> Int64 -> IO ()
_sendFile out_fd in_fd off count = do
    in_hdl <- get_osfhandle in_fd
    rtransmitFile out_fd in_hdl off count

get_osfhandle :: Fd        -- ^ User file descriptor.
              -> IO HANDLE -- ^ The operating-system file handle.
get_osfhandle fd = do
    res <- throwErrnoIf
             (== (#const INVALID_HANDLE_VALUE))
             "Network.Socket.SendFile.Win32.get_osfhandle"
             (c_get_osfhandle fd)
    return (intPtrToPtr res)

setFilePointerEx
    :: HANDLE    -- ^ the handle to set the pointer on
    -> Int64     -- ^ the offset to set the pointer to
    -> DWORD     -- ^ the move method
    -> IO Int64  -- ^ the new absolute offset
setFilePointerEx hdl off meth = alloca $ \res -> do
    failIfZero
      "Network.Socket.SendFile.Win32.setFilePointerEx"
      (c_SetFilePointerEx hdl off res meth)
    peek res

rtransmitFile :: SOCKET -> HANDLE -> Int64 -> Int64 -> IO ()
rtransmitFile _      _      _   0         = return ()
rtransmitFile out_fd in_hdl off remaining = do
    let bytes = min remaining maxBytes
    setFilePointerEx in_hdl off (#const FILE_BEGIN)
    transmitFile out_fd in_hdl (fromIntegral bytes)
    rtransmitFile out_fd in_hdl (off + bytes) (remaining - bytes)

transmitFile :: SOCKET -- ^ A handle to a connected socket.
             -> HANDLE -- ^ A handle to the open file that the TransmitFile function transmits.
             -> DWORD  -- ^ The number of bytes in the file to transmit.
             -> IO ()
transmitFile out_fd in_hdl count = do
    failIfZero
      "Network.Socket.SendFile.Win32.transmitFile"
      (c_TransmitFile out_fd in_hdl count 0 nullPtr nullPtr (#{const TF_USE_KERNEL_APC} .|. #{const TF_WRITE_BEHIND}))
    return ()
    -- according to msdn:
    --   If the TransmitFile function is called with the lpOverlapped parameter
    --   set to NULL, the operation is executed as synchronous I/O. The function
    --   will not complete until the file has been sent.

-- max num of bytes in one send
-- Windows will complain of an "invalid argument" if you use the maxBound of a DWORD, despite the fact that the count parameter is a DWORD; so the upper bound of a 32-bit integer seems to be the real limit, similar to Linux.
maxBytes :: Int64
maxBytes = fromIntegral (maxBound :: Int32)
-- maxBytes = 32 * 1024

-- http://support.microsoft.com/kb/99173 - MAY BE IMPORTANT
-- http://msdn.microsoft.com/en-us/library/ks2530z6.aspx
foreign import ccall unsafe "io.h _get_osfhandle"
    c_get_osfhandle :: Fd
                    -> IO IntPtr

-- http://msdn.microsoft.com/en-us/library/aa365541(VS.85).aspx
foreign import stdcall unsafe "windows.h SetFilePointerEx" c_SetFilePointerEx
    :: HANDLE -> Int64 -> Ptr Int64 -> DWORD -> IO CInt

-- http://msdn.microsoft.com/en-us/library/ms740565(VS.85).aspx
foreign import stdcall safe "mswsock.h TransmitFile"
    c_TransmitFile :: SOCKET
                   -> HANDLE
                   -> DWORD
                   -> DWORD
                   -> Ptr ()
                   -> Ptr ()
                   -> DWORD
                   -> IO CInt
