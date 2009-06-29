import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Data.ByteString.Char8 (ByteString, hGetContents, hPut, pack)
import Network (PortID(..), Socket, accept, connectTo, listenOn, sClose, socketPort)
import Prelude hiding (length)
import SendFile (sendFile)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.IO (Handle, hClose, openBinaryTempFile)
import Test.QuickCheck
import Test.QuickCheck.Monadic

instance Arbitrary ByteString where
  arbitrary = pack `fmap` arbitrary

-- | Accepts a client connection with the passed function.
acceptWith :: Socket            -- ^ The socket providing the service
           -> (Handle -> IO ()) -- ^ This function should take the accepted client connection
           -> IO ByteString     -- ^ Returns what the client received (if anything)
acceptWith service fun = do
    forkIO $ bracket (accept' service) hClose fun
    port <- socketPort service
    hGetContents =<< connectTo "127.0.0.1" port

setup :: IO Socket
setup = do
    createDirectoryIfMissing True "tmp"
    listenOn (PortNumber 61234)

teardown :: Socket -> IO ()
teardown service = do
    removeDirectoryRecursive "tmp"
    sClose service

prop_PayloadArrives :: Socket -> ByteString -> Property
prop_PayloadArrives service payload = monadicIO $ do
    payload' <- run $ acceptWith service sendTempFile
    assert $ payload == payload'
    where sendTempFile peer = sendFile peer =<< createTempFile
          createTempFile =
              bracket
                (openBinaryTempFile "tmp" "test.txt")
                (hClose . snd)
                (\(fp,fh) -> (hPut fh payload) >> return fp)

main =
    bracket
      setup
      teardown
      (\svc -> quickCheckWith args (prop_PayloadArrives svc))
    where args = stdArgs {maxSuccess=200, maxSize=1000}

accept' service = fmap (\(h,_,_) -> h) (accept service)
