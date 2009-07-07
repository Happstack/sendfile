{-# LANGUAGE ForeignFunctionInterface #-}
-- | Linux system-dependent code for 'sendfile'.
module Network.Socket.SendFile.Linux (sendFile') where
import Foreign.C
import GHC.IOBase (haFD)
import GHC.Handle (withHandle_)

sendFile' :: Handle -> FilePath -> Int -> IO ()
sendFile' outh infp offset count = do
    -- flush outh before handing it sendFile
    hFlush outh
    withHandle_ "sendFile" outh $ \outh' -> do 
    withCString infp $ \in_fp -> do
    let out_fd = haFD outh'
    err <- c_sendfile_linux out_fd in_fp (fromIntegral offset) (fromIntegral count)
    if err == 0
        then return ()
        else fail ("errno " ++ show err)

foreign import ccall
    c_sendfile_linux :: CInt -> CString -> CLong -> CLong -> IO Int
