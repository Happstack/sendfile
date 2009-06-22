{-# LANGUAGE ForeignFunctionInterface #-}
module SendFile 
    ( sendFile
    ) where
    
import Foreign.C
import System.IO
import GHC.IOBase
import GHC.Handle

sendFile :: Handle -> FilePath -> IO Bool
sendFile hdl fp = withHandle "sendFile" hdl $ \hdl' -> do
    let clientFd = haFD hdl'
    success <- withCString fp (c_sendfile clientFd)
    return (hdl', success)
    
foreign import ccall unsafe
  c_sendfile :: CInt -> CString -> IO Bool
