import Control.Concurrent (forkIO, myThreadId)
import Control.Exception (SomeException(..), catch, bracket, throw, throwTo, try)
import GHC.IOBase (IOErrorType(..), IOException(..))
import Data.ByteString.Char8 (append, empty, cons, ByteString, drop, hGet, hPut, length, pack, take, unfoldrN)
import Network (PortID(..), Socket, accept, connectTo, listenOn, sClose, socketPort)
import Prelude hiding (catch, drop, length, take)
import Network.Socket.SendFile (sendFile, sendFile', sendFileMode)
import SocketPair (prop_PairConnected, socketPair)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.IO (BufferMode(..), IOMode(..), SeekMode(..), Handle, hClose, hFlush, hSeek, openBinaryTempFile, openBinaryFile, hSetBuffering)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

testWith pair =
    [ testGroup "Test Support"
        [ testProperty "Socket Pair Connected" (prop_PairConnected pair)
        ]
    , testGroup "sendFile (unbuffered)"
        [ testProperty "Payload Arrives" (prop_PayloadArrives pair NoBuffering)
        , testProperty "Payload Arrives In Order" (prop_PayloadArrivesInOrder pair NoBuffering)
        ]
    , testGroup "sendFile (buffered)"
        [ testProperty "Payload Arrives" (prop_PayloadArrives pair (BlockBuffering Nothing))
        , testProperty "Payload Arrives In Order" (prop_PayloadArrivesInOrder pair (BlockBuffering Nothing))
        ]
    , testGroup "sendFile' (unbuffered)"
        [ testProperty "Partial Payload Arrives" (prop_PartialPayloadArrives pair NoBuffering)
        , testProperty "Partial Payload With Seek Arrives" (prop_PartialPayloadWithSeekArrives pair NoBuffering)
        ]
    , testGroup "sendFile' (buffered)"
        [ testProperty "Partial Payload Arrives" (prop_PartialPayloadArrives pair (BlockBuffering Nothing))
        , testProperty "Partial Payload With Seek Arrives" (prop_PartialPayloadWithSeekArrives pair (BlockBuffering Nothing))
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

prop_PayloadArrives :: (Handle, Handle) -> BufferMode -> ByteString -> Property
prop_PayloadArrives (p1, p2) bufMode payload = monadicIO $ do
    run (hSetBuffering p1 bufMode)
    let count = length payload
    fp <- run $ createTempFile payload
    run (sendFile p1 fp) 
    payload' <- run (hGet p2 count)
    assert (payload == payload')

-- see if ordering is correct when interleaving with haskell handle operations
prop_PayloadArrivesInOrder :: (Handle, Handle) -> BufferMode -> ByteString -> Property
prop_PayloadArrivesInOrder (p1, p2) bufMode payload = monadicIO $ do
    run (hSetBuffering p1 bufMode)
    let count = length payload
    fp <- run $ createTempFile payload
    run (hPut p1 beg)
    run (sendFile p1 fp) 
    run (hPut p1 end)
    run (hFlush p1) -- flush after last put
    payload' <- run (hGet p2 (count + length beg + length end))
    assert ((beg `append` payload `append` end) == payload')
    where beg = (pack "BEGINNING")
          end = (pack "END")

prop_PartialPayloadArrives :: (Handle, Handle) -> BufferMode -> ByteString -> Property
prop_PartialPayloadArrives (p1, p2) bufMode payload = monadicIO $ do
    run (hSetBuffering p1 bufMode)
    let count = length payload `div` 2
    fp <- run $ createTempFile payload
    fd <- run $ openBinaryFile fp ReadMode
    run (sendFile' p1 fd (fromIntegral count))
    payload' <- run (hGet p2 count) 
    assert (take count payload == payload')

prop_PartialPayloadWithSeekArrives :: (Handle, Handle) -> BufferMode -> ByteString -> Property
prop_PartialPayloadWithSeekArrives (p1, p2) bufMode payload = monadicIO $ do
    run (hSetBuffering p1 bufMode)
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
