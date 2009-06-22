module SendFile (
    sendFile,
    sendFileMode
    ) where
    
import qualified SendFile.Internal
import System.IO (Handle(..))

-- | A cross-platform wrapper for sendfile -- this implements an available operating-system call if supported, otherwise it falls back to a portable haskell implementation.
sendFile :: Handle   -- ^ The peer Handle to write the file to
         -> FilePath -- ^ The path where the file resides
         -> IO Bool  -- ^ Was the transmission successful?
sendFile = SendFile.Internal.sendFile

-- | Returns the mode that sendfile was compiled with. Mainly for debugging use. Possible values are 'WIN32' or 'PORTABLE'.
sendFileMode :: String -- ^ The mode that sendfile was compiled with
sendFileMode = SendFile.Internal.sendFileMode
