{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SendFile.Internal (
    sendFile,
    sendFileMode
    ) where
    
import Data.ByteString.Char8
import Prelude hiding (readFile)
import System.IO (Handle(..), hFlush)

#if defined(WIN32_SENDFILE) && !defined(PORTABLE_SENDFILE)
import Foreign.C
import GHC.IOBase (haFD)
import GHC.Handle (withHandle_)

sendFileMode :: String
sendFileMode = "WIN32_SENDFILE"

sendFile :: Handle -> FilePath -> IO ()
sendFile outh infp = do
    -- flush outh before handing it sendFile
    hFlush outh
    withHandle_ "sendFile" outh $ \outh' -> do 
    withCString infp $ \in_fp -> do
    let out_fd = haFD outh'
    err <- c_sendfile_win32 out_fd in_fp
    if err == 0
        then return ()
        else fail ("system error " ++ show err)
    
foreign import ccall
    c_sendfile_win32 :: CInt -> CString -> IO Int
#else
#  if defined(LINUX_SENDFILE) && !defined(PORTABLE_SENDFILE)
import Foreign.C
import GHC.IOBase (haFD)
import GHC.Handle (withHandle_)

sendFileMode :: String
sendFileMode = "LINUX_SENDFILE"

sendFile :: Handle -> FilePath -> IO ()
sendFile outh infp = do
    -- flush outh before handing it sendFile
    hFlush outh
    withHandle_ "sendFile" outh $ \outh' -> do 
    withCString infp $ \in_fp -> do
    let out_fd = haFD outh'
    err <- c_sendfile_linux out_fd in_fp
    if err == 0
        then return ()
        else fail ("errno " ++ show err)
    
foreign import ccall
    c_sendfile_linux :: CInt -> CString -> IO Int
#  else
sendFileMode :: String
sendFileMode = "PORTABLE_SENDFILE"

-- FIXME: possibly immature / inefficient implementation
sendFile :: Handle -> FilePath -> IO ()
sendFile outh infp = do
    hPutStr outh =<< readFile infp
    return ()
#  endif
#endif

