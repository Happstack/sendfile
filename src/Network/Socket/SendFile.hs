-- | A cross-platform wrapper for sendfile -- this implements an available operating-system call if supported, otherwise it falls back to a portable haskell implementation. SendFile will flush the output Handle before transmission.
--
--   Keep in mind that the input should only be a `Handle` derived from a network socket (by using socketToHandle). Using something other than a network socket for the input Handle will result in undefined behavior. Furthermore, for consistent read/write behavior, both the output and input handles should be opened in Binary mode rather than Text mode (especially if you are using hSeek).
module Network.Socket.SendFile (
    sendFile,
    sendFile',
    sendFileMode
    ) where
    
import qualified Network.Socket.SendFile.Internal (sendFile, sendFile', sendFileMode)
import System.IO (Handle)

-- | The simplest interface. Simply give it an output `Handle` and the `FilePath` to the input file.
sendFile :: Handle   -- ^ The output handle
         -> FilePath -- ^ The path where the input file resides
         -> IO ()
sendFile = Network.Socket.SendFile.Internal.sendFile

-- | A more powerful interface than sendFile, sendFile' accepts a `Handle` for the input file instead of a `FilePath` and the number of bytes you would like to read; this number must be a positive integer. This unlocks the full potential `Handle`(s). For instance, if you wanted to start reading from a particular offset, you could utilize `hSeek`. If you need the file size you can use 'hFileSize'.
sendFile' :: Handle  -- ^ The output handle
          -> Handle  -- ^ The input handle
          -> Integer -- ^ The number of bytes to read
          -> IO ()
sendFile' = Network.Socket.SendFile.Internal.sendFile'

-- | Returns the mode that sendfile was compiled with. Mainly for debugging use. Possible values are 'WIN32_SENDFILE', 'LINUX_SENDFILE', 'FREEBSD_SENDFILE', and 'PORTABLE_SENDFILE'.
sendFileMode :: String -- ^ The mode that sendfile was compiled with
sendFileMode = Network.Socket.SendFile.Internal.sendFileMode
