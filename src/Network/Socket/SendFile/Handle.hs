-- | Handle-based versions of some of the functions exported by
-- Network.Socket.SendFile.  None of the unsafe functions are exposed here.
--

module Network.Socket.SendFile.Handle (
  ByteCount,
  Offset,
  Iter(..), runIter,
  -- * Handle-based sendFiles
  sendFile,
  sendFileIterWith,
  sendFile',
  sendFileIterWith'
  ) where

import System.IO (Handle, hFileSize)

import qualified Network.Socket.SendFile.Internal as Internal
import Network.Socket.SendFile.Iter (Iter(..), runIter)
import Network.Socket.SendFile (ByteCount, Offset)
import Network.Socket (Socket)

-- | Simple sendFile - give it a Socket and a Handle, and it sends the entire
-- file through the socket.
sendFile
  :: Socket
  -> Handle
  -> IO ()
sendFile outs inh = do
  count <- hFileSize inh
  Internal.sendFile'' outs inh 0 count

-- | A more interactive version of sendFile, which accepts a callback function
-- in addition to the socket and handle.  The callback will be called for each
-- chunk of data the sendFileIterWith function acts on.
sendFileIterWith
  :: (IO Iter -> IO a)
  -> Socket
  -> Handle
  -> ByteCount
  -> IO a
sendFileIterWith stepper outs inh blockSize = do
  count <- hFileSize inh
  Internal.sendFileIterWith'' stepper outs inh blockSize 0 count

-- | A sendFile that allows the user to send a subset of the file associated
-- with the given handle.
sendFile'
  :: Socket
  -> Handle
  -> Offset
  -> ByteCount
  -> IO ()
sendFile' = Internal.sendFile''

-- | A more powerful version of sendFileIterWith, which allows the sending of a
-- subset of the given file.
sendFileIterWith'
  :: (IO Iter -> IO a)
  -> Socket
  -> Handle
  -> ByteCount
  -> Offset
  -> ByteCount
  -> IO a
sendFileIterWith' = Internal.sendFileIterWith''

