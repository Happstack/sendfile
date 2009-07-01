import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Data.ByteString.Char8 (empty, cons, ByteString, drop, hGet, hPut, length, pack, take, unfoldrN)
import Network (PortID(..), Socket, accept, connectTo, listenOn, sClose, socketPort)
import Prelude hiding (drop, length, take)
import SendFile (sendFile, sendFile', sendFileMode)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.IO (Handle, hClose, openBinaryTempFile)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

instance Arbitrary ByteString where
    arbitrary = fmap pack arbitrary

testWith svc =
    [ testGroup "Sorting Group 1"
        [ testProperty "Payload Arrives" (prop_PayloadArrives svc)
        , testProperty "Partial Payload Arrives" (prop_PartialPayloadArrives svc)
        , testProperty "Partial Offset Payload Arrives" (prop_PartialOffsetPayloadArrives svc)
        ]
    ]

main = do
    putStrLn sendFileMode
    bracket setup teardown (defaultMain . testWith)

{-
main = do

    where args = stdArgs {maxSuccess=50, maxSize=50*kB}

kB = 1024
-}

-- | Accepts a client connection with the passed function. Returns data read from server
acceptWith :: Socket            -- ^ The socket providing the service
           -> (Handle -> IO ()) -- ^ This function should take the accepted client connection
           -> Int               -- ^ The amount of bytes that should be read from the server
           -> IO ByteString     -- ^ Returns what the client received (if anything)
acceptWith service fun len = do
    forkIO $ bracket (accept' service) hClose fun
    port <- socketPort service
    peer <- connectTo "127.0.0.1" port
    hGet peer len

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
    let count = length payload
    payload' <- run $ acceptWith service (\peer -> sendFile peer =<< createTempFile payload) count
    assert $ payload == payload'
    
prop_PartialPayloadArrives :: Socket -> ByteString -> Property
prop_PartialPayloadArrives service payload = monadicIO $ do
    let count = length payload `div` 2
    payload' <- run $ acceptWith service (\peer -> ((\fp -> sendFile' peer fp 0 count) =<< createTempFile payload)) count
    assert $ take count payload == payload'
    
prop_PartialOffsetPayloadArrives :: Socket -> ByteString -> Property
prop_PartialOffsetPayloadArrives service payload = monadicIO $ do
    let offset = length payload `div` 2
        count = (length payload) - offset
    payload' <- run $ acceptWith service (\peer -> ((\fp -> sendFile' peer fp offset count) =<< createTempFile payload)) count
    assert $ drop offset payload == payload'

accept' :: Socket -> IO Handle
accept' service = fmap (\(h,_,_) -> h) (accept service)

createTempFile :: ByteString -> IO FilePath
createTempFile payload =
    bracket
      (openBinaryTempFile "tmp" "test.txt")
      (hClose . snd)
      (\(fp,fh) -> (hPut fh payload) >> return fp)
