{-# LANGUAGE ForeignFunctionInterface #-}
-- | Darwin system-dependent code for 'sendfile'.
module Network.Socket.SendFile.Darwin (_sendFile) where
import Data.Int (Int64)
import Foreign.C.Error (eAGAIN, getErrno, throwErrno)
import Foreign.C.Types (CInt)
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import System.Posix.Types (Fd)
import Control.Concurrent (threadWaitWrite)

{-
  This is based on Linux.hsc (from version 0.5), although the
  calling convention for Darwin's sendfile() is closer to that
  of FreeBSD.
-}

_sendFile :: Fd -> Fd -> Int64 -> Int64 -> IO ()
_sendFile out_fd in_fd off count = do
  alloca $ \pbytes -> do
    rsendfile out_fd in_fd pbytes off count

rsendfile :: Fd -> Fd -> Ptr Int64 -> Int64 -> Int64 -> IO ()
rsendfile _      _     _      _    0        = return ()
rsendfile out_fd in_fd pbytes off remaining = do
  let bytes = min remaining maxBytes
  sbytes <- sendfile out_fd in_fd pbytes off bytes 0
  rsendfile out_fd in_fd pbytes off (remaining - sbytes)

{-
 Unlike the Linux implementation we track any re-sends due
 to getting eAGAIN. If this wasn't done then the call would
 appear to hang. The alternative would be to just 
    return bytes
 but I am unclear whether there are occasions this may
 be wrong.
-}

sendfile :: Fd -> Fd -> Ptr Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
sendfile out_fd in_fd pbytes off bytes ctr = do
  poke pbytes bytes
  -- Copying Linux implementation for thread synchronization,
  -- is this sensible?
  threadWaitWrite out_fd
  status <- c_sendfile out_fd in_fd off pbytes
  nsent <- peek pbytes
  if status == 0
    then return $ (fromIntegral nsent) + ctr
    else do errno <- getErrno
            if errno == eAGAIN
              -- is it correct to reduce bytes by nsent here?
              then let nremaining = max 0 (bytes - (fromIntegral nsent))
                   in sendfile out_fd in_fd pbytes off nremaining (ctr + (fromIntegral nsent))
              else throwErrno "Network.Socket.SendFile.Darwin"

-- max num of bytes in one send
maxBytes :: Int64
maxBytes = fromIntegral (maxBound :: (#type off_t))

-- in Darwin sendfile gives LFS support (no sendfile64 routine)
foreign import ccall unsafe "sys/uio.h sendfile" c_sendfile_darwin
    :: Fd -> Fd -> (#type off_t) -> Ptr (#type off_t) -> Ptr () -> CInt -> IO CInt

c_sendfile :: Fd -> Fd -> (#type off_t) -> Ptr (#type off_t) -> IO CInt
c_sendfile out_fd in_fd off pbytes = c_sendfile_darwin in_fd out_fd off pbytes nullPtr 0
