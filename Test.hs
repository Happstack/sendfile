import Control.Concurrent (forkIO, myThreadId)
import Control.Exception (SomeException(..), catch, bracket, throw, throwTo, try)
import GHC.IOBase (IOErrorType(..), IOException(..))
import Data.ByteString.Char8 (empty, cons, ByteString, drop, hGet, hPut, length, pack, take, unfoldrN)
import Network (PortID(..), Socket, accept, connectTo, listenOn, sClose, socketPort)
import Prelude hiding (catch, drop, length, take)
import Network.Socket.SendFile (sendFile, sendFile', sendFileMode)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.IO (IOMode(..), SeekMode(..), Handle, hClose, hFlush, hSeek, openBinaryTempFile, openBinaryFile)
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

catch' :: IO a -> (SomeException -> IO a) -> IO a
catch' = catch

-- | Accepts a client connection with the passed function. Returns data read from server
acceptWith :: Socket            -- ^ The socket providing the service
           -> (Handle -> IO ()) -- ^ This function should take the accepted client connection
           -> Int               -- ^ The amount of bytes that should be read from the server
           -> IO ByteString     -- ^ Returns what the client received (if anything)
acceptWith service fun len = do
    parent <- myThreadId
    forkIO $ catch' (bracket (accept' service) hClose fun) (throwTo parent)
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
    fp <- run $ createTempFile payload
    res <- run $ (try $ acceptWith service (\peer -> sendFile peer fp) count :: IO (Either IOException ByteString))
    case res of
        Right payload' -> assert $ payload == payload'
        Left IOError {ioe_type=InvalidArgument} -> pre False
        Left e @ (IOError {}) -> run $ putStrLn (show $ ioe_type e) >> throw e
    
prop_PartialPayloadArrives :: Socket -> ByteString -> Property
prop_PartialPayloadArrives service payload = monadicIO $ do
    let count = length payload `div` 2
    fp <- run $ createTempFile payload
    fd <- run $ openBinaryFile fp ReadMode
    res <- run $ (try $ acceptWith service (\peer -> sendFile' peer fd (fromIntegral count)) count :: IO (Either IOException ByteString))
    case res of
        Right payload' -> assert $ take count payload == payload'
        Left IOError {ioe_type=InvalidArgument} -> pre False
        Left e @ (IOError {}) -> run $ putStrLn (show $ ioe_type e) >> throw e
    
prop_PartialOffsetPayloadArrives :: Socket -> ByteString -> Property
prop_PartialOffsetPayloadArrives service payload = monadicIO $ do
    let offset = length payload `div` 2
        count = (length payload) - offset
    fp <- run $ createTempFile payload
    fd <- run $ openBinaryFile fp ReadMode
    run $ hSeek fd AbsoluteSeek (fromIntegral offset)
    res <- run $ (try $ acceptWith service (\peer -> sendFile' peer fd (fromIntegral count)) count :: IO (Either IOException ByteString))
    case res of
        Right payload' -> assert $ drop offset payload == payload'
        Left IOError {ioe_type=InvalidArgument} -> pre False
        Left e @ (IOError {}) -> run $ putStrLn (show $ ioe_type e) >> throw e
    

accept' :: Socket -> IO Handle
accept' service = fmap (\(h,_,_) -> h) (accept service)

createTempFile :: ByteString -> IO FilePath
createTempFile payload =
    bracket
      (openBinaryTempFile "tmp" "test.txt")
      (hClose . snd)
      (\(fp,fh) -> hPut fh payload >> return fp)
