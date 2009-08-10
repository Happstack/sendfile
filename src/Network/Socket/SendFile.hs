-- | A cross-platform wrapper for sendfile -- this implements an available operating-system call if supported, otherwise it falls back to a portable haskell implementation.
--
--   Two interfaces are provided for both the unsafe and safe sets of functions. The first interface accepts an output socket\/handle and the path of the file you want to send; sendFile and unsafeSendFile comprise this interface. The second interface accepts an output socket\/handle, a handle to the file you want to send, an offset, and the number of bytes you want to send; sendFile' and unsafeSendFile' comprise this interface.
--
--   For consistent read/write behavior with either sendFile' or unsafeSendFile', the input handle should be opened in Binary mode rather than Text mode.
--

module Network.Socket.SendFile (
    ByteCount,
    Offset,
    -- * Safe functions (recommended)
    sendFile,
    sendFile',
    -- * Unsafe functions (not recommended)
    -- | With unsafeSendFile and unsafeSendFile' the output should only be a `Handle` derived from a network socket (by using socketToHandle). These functions are unsafe because you cannot know whether or not a Handle is actually a `Socket` at compile-time. If you try to use  a `Handle` for the output which is not in fact a socket, you will get a runtime error.
    --
    --   These functions are provided for convenience given that the current high-level interface in the Network package gives you a `Handle` when you use accept. They may be deprecated in the future.
    
    unsafeSendFile,
    unsafeSendFile',
    -- * Utility functions
    sendFileMode
    ) where
import qualified Network.Socket.SendFile.Internal (sendFile, sendFile', sendFileMode, unsafeSendFile, unsafeSendFile')
import Network.Socket (Socket)
import System.IO (Handle)

-- | The file offset (in bytes) to start from
type Offset = Integer

-- | The length (in bytes) which should be sent
type ByteCount = Integer

-- | The simplest interface. Simply give it an output `Socket` and the `FilePath` to the input file.
sendFile
    :: Socket   -- ^ The output socket
    -> FilePath -- ^ The path where the input file resides
    -> IO ()
sendFile = Network.Socket.SendFile.Internal.sendFile

-- | A more powerful interface than sendFile, sendFile' accepts a `Handle` for the input file instead of a `FilePath`, a starting offset, and the bytecount to send; the offset and the count must be a positive integer. The initial position of the input file handle matters not since the offset is absolute, and the final position may be different depending on the platform -- no assumptions can be made.
sendFile'
    :: Socket    -- ^ The output socket
    -> Handle    -- ^ The input file handle
    -> Offset    -- ^ The offset to start at
    -> ByteCount -- ^ The number of bytes to send
    -> IO ()
sendFile' = Network.Socket.SendFile.Internal.sendFile'

-- | The unsafe version of sendFile which accepts a `Handle` instead of a `Socket` for the output.  It will flush the output handle before sending any file data.
unsafeSendFile
    :: Handle   -- ^ The output handle
    -> FilePath -- ^ The path where the input file resides
    -> IO ()
unsafeSendFile = Network.Socket.SendFile.Internal.unsafeSendFile

-- | The unsafe version of sendFile' which accepts a `Handle` instead of a `Socket` for the output. It will flush the output handle before sending any file data.
unsafeSendFile'
    :: Handle    -- ^ The output handle
    -> Handle    -- ^ The input file handle
    -> Offset    -- ^ The offset to start at
    -> ByteCount -- ^ The number of bytes to send
    -> IO ()
unsafeSendFile' = Network.Socket.SendFile.Internal.unsafeSendFile'

-- | Returns the mode that sendfile was compiled with. Mainly for debugging use.
-- | Possible values are 'WIN32_SENDFILE', 'LINUX_SENDFILE', 'FREEBSD_SENDFILE',
-- | 'DARWIN_SENDFILE', and 'PORTABLE_SENDFILE'.
sendFileMode :: String -- ^ The mode that sendfile was compiled with
sendFileMode = Network.Socket.SendFile.Internal.sendFileMode

