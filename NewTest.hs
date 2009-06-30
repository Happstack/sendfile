import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Data.ByteString.Char8 (empty, cons, ByteString, drop, hGetContents, hPut, length, pack, take, unfoldrN)
import Network (PortID(..), Socket, accept, connectTo, listenOn, sClose, socketPort)
import Prelude hiding (drop, length, take)
import SendFile (sendFile, sendFile', sendFileMode)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.IO (Handle, hClose, openBinaryTempFile)
import Test.QuickCheck
import Test.QuickCheck.Monadic

instance Arbitrary ByteString where
    arbitrary = fmap pack arbitrary

main = do
    putStrLn sendFileMode
    bracket
      setup
      teardown
      (\svc -> do putStrLn "prop_PayloadArrives" >> quickCheckWith args (prop_PayloadArrives svc)
                  putStrLn "prop_PartialPayloadArrives" >> quickCheckWith args (prop_PartialPayloadArrives svc)
                  putStrLn "prop_PartialOffsetPayloadArrives" >> quickCheckWith args (prop_PartialOffsetPayloadArrives svc))
    where args = stdArgs {maxSuccess=50, maxSize=50*kB}

kB = 1024

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
    payload' <- run $ acceptWith service (\peer -> sendFile peer =<< createTempFile payload)
    assert $ payload == payload'
    
prop_PartialPayloadArrives :: Socket -> ByteString -> Property
prop_PartialPayloadArrives service payload = monadicIO $ do
    let count = length payload `div` 2
    payload' <- run $ acceptWith service (\peer -> ((\fp -> sendFile' peer fp 0 count) =<< createTempFile payload))
    assert $ take count payload == payload'
    
prop_PartialOffsetPayloadArrives :: Socket -> ByteString -> Property
prop_PartialOffsetPayloadArrives service payload = monadicIO $ do
    let offset = length payload `div` 2
        count = (length payload) - offset
    payload' <- run $ acceptWith service (\peer -> ((\fp -> sendFile' peer fp offset count) =<< createTempFile payload))
    assert $ drop offset payload == payload'

accept' :: Socket -> IO Handle
accept' service = fmap (\(h,_,_) -> h) (accept service)

createTempFile :: ByteString -> IO FilePath
createTempFile payload =
    bracket
      (openBinaryTempFile "tmp" "test.txt")
      (hClose . snd)
      (\(fp,fh) -> (hPut fh payload) >> return fp)
