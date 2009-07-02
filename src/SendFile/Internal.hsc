{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SendFile.Internal (
    fileSize,
    sendFile,
    sendFile',
    sendFileMode
    ) where

import System.IO (
    Handle(..),
    IOMode(..),
    SeekMode(..),
    hFileSize,
    hFlush,
    hSeek,
    withBinaryFile
    )

#if defined(WIN32_SENDFILE)
import Foreign.C
import GHC.IOBase (haFD)
import GHC.Handle (withHandle_)

sendFileMode :: String
sendFileMode = "WIN32_SENDFILE"

sendFile' :: Handle -> FilePath -> Int -> Int -> IO ()
sendFile' outh infp offset count = do
    -- flush outh before handing it sendFile
    hFlush outh
    withHandle_ "sendFile" outh $ \outh' -> do 
    withCString infp $ \in_fp -> do
    let out_fd = haFD outh'
    err <- c_sendfile_win32 out_fd in_fp (fromIntegral offset) (fromIntegral count)
    if err == 0
        then return ()
        else fail ("system error " ++ show err)
    
foreign import ccall
    c_sendfile_win32 :: CInt -> CString -> CLong -> CLong -> IO Int
#endif

#if defined(LINUX_SENDFILE)
import Foreign.C
import GHC.IOBase (haFD)
import GHC.Handle (withHandle_)

sendFileMode :: String
sendFileMode = "LINUX_SENDFILE"

sendFile' :: Handle -> FilePath -> Int -> Int -> IO ()
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
#endif

#if defined(PORTABLE_SENDFILE)
import Prelude hiding (readFile)
import Data.ByteString.Char8

sendFileMode :: String
sendFileMode = "PORTABLE_SENDFILE"

-- FIXME: possibly immature / inefficient implementation
sendFile' :: Handle -> FilePath -> Int -> Int -> IO ()
sendFile' outh infp offset count =
    withBinaryFile infp ReadMode $ \inh -> do
    hSeek inh AbsoluteSeek (fromIntegral offset)
    hPutStr outh =<< hGet inh count
    hFlush outh 
    return ()
#endif

fileSize :: FilePath -> IO Int
fileSize fp = fmap fromIntegral (withBinaryFile fp ReadMode hFileSize)

sendFile :: Handle -> FilePath -> IO ()
sendFile outh infp = do
    count <- fileSize infp
    sendFile' outh infp 0 count

