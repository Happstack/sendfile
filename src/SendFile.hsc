{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SendFile 
    ( sendFile
    , sendFileMode
    ) where

import Prelude hiding (readFile)
import System.IO (Handle(..))
import System.IO.Strict

#if defined(PORTABLE_SENDFILE)
sendFileMode = "PORTABLE"

sendFile :: Handle -> FilePath -> IO Bool
sendFile = portableSendFile

#else
#  if defined(WIN32_SENDFILE)
import Foreign.C
import GHC.IOBase
import GHC.Handle

sendFileMode = "WIN32"

sendFile :: Handle -> FilePath -> IO Bool
sendFile hdl fp = withHandle "sendFile" hdl $ \hdl' -> do
    let clientFd = haFD hdl'
    success <- withCString fp (c_sendfile clientFd)
    return (hdl', success)
    
foreign import ccall unsafe
    c_sendfile :: CInt -> CString -> IO Bool
    
#  else
sendFileMode = "PORTABLE"

sendFile :: Handle -> FilePath -> IO Bool
sendFile = portableSendFile

#  endif
#endif

-- FIXME: immature / inefficient implementation
portableSendFile :: Handle -> FilePath -> IO Bool
portableSendFile hdl fp = run $ do
    hPutStr hdl =<< readFile fp
    return True
