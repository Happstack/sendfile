import Control.Concurrent (forkIO, myThreadId)
import Control.Exception (SomeException(..), catch, bracket, throw, throwTo, try)
import GHC.IOBase (IOErrorType(..), IOException(..))
import Data.ByteString.Char8 (empty, cons, ByteString, drop, hGet, hPut, length, pack, take, unfoldrN)
import Network (PortID(..), Socket, accept, connectTo, listenOn, sClose, socketPort)
import Prelude hiding (catch, drop, length, take)
import Network.Socket.SendFile (sendFile, sendFile', sendFileMode)
import SocketPair (prop_PairConnected, socketPair)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.IO (IOMode(..), SeekMode(..), Handle, hClose, hFlush, hSeek, openBinaryTempFile, openBinaryFile)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- Test TODOS
--   > Ensure that ordering before / after sendFile with haskell handles is always preserved
--   > Ensure that negative integers always cause an exception (HUnit?)
testWith pair =
    [ testGroup "Test Support"
        [ testProperty "Socket Pair Connected" (prop_PairConnected pair)
        ]
    , testGroup "sendFile"
        [ testProperty "Payload Arrives" (prop_PayloadArrives pair)
        ]
    , testGroup "sendFile'"
        [ testProperty "Partial Payload Arrives" (prop_PartialPayloadArrives pair)
        , testProperty "Partial Payload With Seek Arrives" (prop_PartialPayloadWithSeekArrives pair)
        ]
    ]
 
main = do
    putStrLn sendFileMode
    bracket setup teardown (defaultMain . testWith)

setup :: IO (Handle, Handle)
setup = do
    createDirectoryIfMissing True "tmp"
    socketPair

teardown :: (Handle, Handle) -> IO ()
teardown (p1, p2) = do
    removeDirectoryRecursive "tmp"
    hClose p1
    hClose p2

prop_PayloadArrives :: (Handle, Handle) -> ByteString -> Property
prop_PayloadArrives (p1, p2) payload = monadicIO $ do
    let count = length payload
    fp <- run $ createTempFile payload
    run (sendFile p1 fp) 
    payload' <- run (hGet p2 count)
    assert (payload == payload')

prop_PartialPayloadArrives :: (Handle, Handle) -> ByteString -> Property
prop_PartialPayloadArrives (p1, p2) payload = monadicIO $ do
    let count = length payload `div` 2
    fp <- run $ createTempFile payload
    fd <- run $ openBinaryFile fp ReadMode
    run (sendFile' p1 fd (fromIntegral count))
    payload' <- run (hGet p2 count) 
    assert (take count payload == payload')

prop_PartialPayloadWithSeekArrives :: (Handle, Handle) -> ByteString -> Property
prop_PartialPayloadWithSeekArrives (p1, p2) payload = monadicIO $ do
    let offset = length payload `div` 2
        count = (length payload) - offset
    fp <- run $ createTempFile payload
    fd <- run $ openBinaryFile fp ReadMode
    run (hSeek fd AbsoluteSeek (fromIntegral offset))
    run (sendFile' p1 fd (fromIntegral count))
    payload' <- run (hGet p2 count)
    assert (drop offset payload == payload')

createTempFile :: ByteString -> IO FilePath
createTempFile payload =
    bracket
      (openBinaryTempFile "tmp" "test.txt")
      (hClose . snd)
      (\(fp,fh) -> hPut fh payload >> return fp)
