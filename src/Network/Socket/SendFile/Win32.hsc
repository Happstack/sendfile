{-# LANGUAGE ForeignFunctionInterface #-}
-- | Win32 system-dependent code for 'TransmitFile'.
module Network.Socket.SendFile.Win32 (_sendFile) where
import Data.Int
import Data.Word
import Foreign.C.Error (throwErrno)
import Foreign.Ptr (IntPtr, intPtrToPtr)
import GHC.Handle (withHandle_)
import GHC.IOBase (FD, haFD)
import System.IO (Handle, hFlush)
import System.Win32.Types

#include <windows.h>

_sendFile :: Handle -> Handle -> Integer -> IO ()
_sendFile outp inp count = do
    hFlush outp -- flush the buffer before invoking transmitFile
    withHandle_ "Network.Socket.SendFile.Win32.sendFile'" outp $ \outp' -> do
    withHandle_ "Network.Socket.SendFile.Win32.sendFile'" inp $ \inp' -> do
    let out_fd = haFD outp'
    in_hdl <- get_osfhandle (haFD inp')
    transmitFile (fromIntegral out_fd) in_hdl (fromIntegral count)

get_osfhandle :: FD        -- ^ User file descriptor.
              -> IO HANDLE -- ^ The operating-system file handle.
get_osfhandle fd = do
    res <- c_get_osfhandle fd
    if res == (#const INVALID_HANDLE_VALUE)
      then throwErrno "Network.Socket.SendFile.Win32.get_osfhandle"
      else return (intPtrToPtr res)

transmitFile :: FD     -- ^ A handle to a connected socket.
             -> HANDLE -- ^ A handle to the open file that the TransmitFile function transmits.
             -> DWORD  -- ^ The number of bytes in the file to transmit.
             -> IO ()
transmitFile out_fd in_hdl count =
    failIf_ (== 0) "Network.Socket.SendFile.Win32.transmitFile" (c_TransmitFile out_fd in_hdl count)
    
foreign import ccall
    c_get_osfhandle :: FD -> IO IntPtr
    
foreign import ccall
    c_TransmitFile :: FD -> HANDLE -> DWORD -> IO (#type BOOL)
