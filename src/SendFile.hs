module SendFile (
    fileSize,
    sendFile,
    sendFile',
    sendFileMode
    ) where
    
import qualified SendFile.Internal
import System.IO (Handle(..))

-- | Get the size of a file.
fileSize :: FilePath -- ^ The path of the file
         -> IO Int   -- ^ The size of the file (in bytes)
fileSize = SendFile.Internal.fileSize

-- | A cross-platform wrapper for sendfile -- this implements an available operating-system call if supported, otherwise it falls back to a portable haskell implementation. It takes a Handle which it will first flush before handing it to the operating system to perform the transmission.
sendFile :: Handle   -- ^ The output Handle
         -> FilePath -- ^ The path where the input file resides
         -> IO ()  -- ^ Whether or not the transmission was successful
sendFile = SendFile.Internal.sendFile

-- | Like sendFile except that it additionally allows you to specify an offset and count. The behavior of reading beyond the end of the file is undefined.
sendFile' :: Handle   -- ^ The output Handle
          -> FilePath -- ^ The path where the input file resides
          -> Int      -- ^ The starting offset of the file (in bytes)
          -> Int      -- ^ The number of bytes to read
          -> IO ()
sendFile' = SendFile.Internal.sendFile'

-- | Returns the mode that sendfile was compiled with. Mainly for debugging use. Possible values are 'WIN32_SENDFILE' and 'PORTABLE_SENDFILE'.
sendFileMode :: String -- ^ The mode that sendfile was compiled with
sendFileMode = SendFile.Internal.sendFileMode
