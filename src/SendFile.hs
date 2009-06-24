module SendFile (
    fileSize,
    sendFile,
    sendFile',
    sendFileMode
    ) where
    
import qualified SendFile.Internal
import System.IO (Handle(..))

-- | A cross-platform wrapper for sendfile -- this implements an available operating-system call if supported, otherwise it falls back to a portable haskell implementation. It takes a Handle which it will first flush before handing it to the operating system to perform the transmission.
sendFile :: Handle   -- ^ The output Handle
         -> FilePath -- ^ The path where the input file resides
         -> IO ()
sendFile = SendFile.Internal.sendFile

-- | Returns the mode that sendfile was compiled with. Mainly for debugging use. Possible values are 'WIN32_SENDFILE' and 'PORTABLE_SENDFILE'.
sendFileMode :: String -- ^ The mode that sendfile was compiled with
sendFileMode = SendFile.Internal.sendFileMode
