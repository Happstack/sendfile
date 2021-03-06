{-# OPTIONS_GHC -Wall #-}
-- [ required libs from hackage ]
-- QuickCheck-2.1.0.1
-- test-framework-quickcheck2
-- test-framework-hunit
-- NOTE: do a 'runhaskell GenLargeFile.hs' first, it is needed for the test
import Control.Concurrent (forkIO)
import Control.Exception (bracket, finally)
import Data.ByteString.Char8 (append, drop, ByteString, hGet, hGetNonBlocking, hPut, length, pack, take)
import Prelude hiding (catch, drop, length, take)
import Network.Socket.SendFile (sendFile, sendFile', sendFileMode, unsafeSendFile, unsafeSendFile')
import Network.Socket.ByteString (recv, sendAll)
import Network.Socket (Socket)
import SocketPair (prop_HandlePairConnected, prop_SocketPairConnected, handlePair, socketPair, recvAll)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.IO (BufferMode(..), IOMode(..), {- SeekMode(..), -} Handle, hClose, hFileSize, hFlush, hSetBuffering, {- hSeek , -} openBinaryTempFile{- , withBinaryFile -}, withFile)
import qualified Test.HUnit as H
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

testWith :: (Socket, Socket) -> (Handle, Handle) -> [Test]
testWith spair hpair =
    [ testGroup "Test Support"
        [ testProperty "Socket Pair Connected" (prop_SocketPairConnected spair)
        , testProperty "Handle Pair Connected" (prop_HandlePairConnected hpair)
        ]

    , testGroup "sendFile"
        [ testProperty "Payload Arrives" (prop_PayloadArrives spair)
        , testProperty "Payload Arrives In Order" (prop_PayloadArrivesInOrder spair)
        ]

    , testGroup "sendFile'"
        [ testProperty "Partial Payload Arrives" (prop_PartialPayloadArrives spair)
        , testProperty "Partial Payload with Offset Arrives" (prop_PartialPayloadWithOffsetArrives spair)
--        , testProperty "Handle Position Ignored" (prop_HandlePositionIgnored spair)
        , testCase "Large Filesize Arrives" (test_LargeFileSizeArrives spair)
        ]

    , testGroup "unsafeSendFile (unbuffered)"
        [ testProperty "Payload Arrives" (prop_UnsafePayloadArrives hpair NoBuffering)
        , testProperty "Payload Arrives In Order" (prop_UnsafePayloadArrivesInOrder hpair NoBuffering)
        ]

    , testGroup "unsafeSendFile (buffered)"
        [ testProperty "Payload Arrives" (prop_UnsafePayloadArrives hpair (BlockBuffering Nothing))
        , testProperty "Payload Arrives In Order" (prop_UnsafePayloadArrivesInOrder hpair (BlockBuffering Nothing))
        ]

    , testGroup "unsafeSendFile' (unbuffered)"
        [ testProperty "Partial Payload Arrives" (prop_UnsafePartialPayloadArrives hpair NoBuffering)
        , testProperty "Partial Payload with Offset Arrives" (prop_UnsafePartialPayloadWithOffsetArrives hpair NoBuffering)
--        , testProperty "Handle Position Ignored" (prop_UnsafeHandlePositionIgnored hpair NoBuffering)
        , testCase "Large Filesize Arrives" (test_UnsafeLargeFileSizeArrives hpair NoBuffering)
        ]

    , testGroup "unsafeSendFile' (buffered)"
        [ testProperty "Partial Payload Arrives" (prop_UnsafePartialPayloadArrives hpair (BlockBuffering Nothing))
        , testProperty "Partial Payload with Offset Arrives" (prop_UnsafePartialPayloadWithOffsetArrives hpair (BlockBuffering Nothing))
--        , testProperty "Handle Position Ignored" (prop_UnsafeHandlePositionIgnored hpair (BlockBuffering Nothing))
        , testCase "Large Filesize Arrives" (test_UnsafeLargeFileSizeArrives hpair (BlockBuffering Nothing))
        ]
    ]

main :: IO ()
main = do
    putStrLn sendFileMode
    createDirectoryIfMissing True "tmp"
    spair <- socketPair
    hpair <- handlePair
    defaultMain (testWith spair hpair)

--------------------------------------------------------------------------------
-- sendFile & sendFile'                                                       --
--------------------------------------------------------------------------------
prop_PayloadArrives :: (Socket, Socket) -> ByteString -> Property
prop_PayloadArrives (p1, p2) payload = monadicIO $ do
    let count = length payload
    run (withTempFile payload $ \fp -> do
             sendFile p1 fp)
    payload' <- run (recvAll p2 count)
    assert (payload == payload')

-- see if ordering is correct when interleaving with haskell socket operations
prop_PayloadArrivesInOrder :: (Socket, Socket) -> ByteString -> Property
prop_PayloadArrivesInOrder (p1, p2) payload = monadicIO $ do
    let count = length payload
    run (withTempFile payload $ \fp -> do
             sendAll p1 beg
             sendFile p1 fp
             sendAll p1 end)
    payload' <- run (recvAll p2 (count + length beg + length end))
    assert ((beg `append` payload `append` end) == payload')
    where beg = (pack "BEGINNING")
          end = (pack "END")

prop_PartialPayloadArrives :: (Socket, Socket) -> ByteString -> Property
prop_PartialPayloadArrives (p1, p2) payload = monadicIO $ do
    count <- pick (choose (0, length payload))
    run (withTempFile payload $ \fp ->
             sendFile' p1 fp 0 (fromIntegral count))
    payload' <- run (recvAll p2 count) 
    assert (take count payload == payload')

prop_PartialPayloadWithOffsetArrives :: (Socket, Socket) -> ByteString -> Property
prop_PartialPayloadWithOffsetArrives (p1, p2) payload = monadicIO $ do
    let len = length payload
    offset <- pick (choose (0, len))
    let count = len - offset
    run (withTempFile payload $ \fp -> do
             sendFile' p1 fp (fromIntegral offset) (fromIntegral count))
    payload' <- run (recvAll p2 count)
    assert (take count (drop offset payload) == payload')
{-
prop_HandlePositionIgnored :: (Socket, Socket) -> ByteString -> Property
prop_HandlePositionIgnored (p1, p2) payload = monadicIO $ do
    let len = length payload
    offset <- pick (choose (0, len))
    randpos <- pick (choose (0, len))
    let count = len - offset
    pre (offset /= randpos)
    run (withTempFile payload $ \fp -> do
         withBinaryFile fp ReadMode $ \fd -> do
             hSeek fd AbsoluteSeek (fromIntegral randpos) -- try to mess up the transmission
             sendFile' p1 fd (fromIntegral offset) (fromIntegral count))
    payload' <- run (recvAll p2 count)
    assert (take count (drop offset payload) == payload')
-}
test_LargeFileSizeArrives :: (Socket, Socket) -> H.Assertion
test_LargeFileSizeArrives (p1, p2) = do
    -- file is assumed to be 3gb, and is already created (use GenLargeFile.hs)
--    withBinaryFile "large.txt" ReadMode $ \h -> do
    largeLen <- withFile "large.txt" ReadMode $ hFileSize
    _ <- forkIO (sendFile' p1 "large.txt" 0 largeLen)
    receivedLen <- recvCountBytes p2 (fromIntegral largeLen)
    H.assertEqual "all bytes arrived" receivedLen (fromIntegral largeLen)
    where -- largeLen = 3 * 1024 * 1024 * 1024
          recvCountBytes _    0 = return 0
          recvCountBytes sock len = do
              recvLen <- fmap length (recv sock 4194304)
              fmap (recvLen +) (recvCountBytes sock (len - recvLen))

--------------------------------------------------------------------------------
-- unsafeSendFile & unsafeSendFile'                                           --
--------------------------------------------------------------------------------

prop_UnsafePayloadArrives :: (Handle, Handle) -> BufferMode -> ByteString -> Property
prop_UnsafePayloadArrives (p1, p2) bufMode payload = monadicIO $ do
    run (hSetBuffering p1 bufMode)
    let count = length payload
    run (withTempFile payload $ \fp -> do
             unsafeSendFile p1 fp)
    payload' <- run (hGet p2 count)
    assert (payload == payload')

-- see if ordering is correct when interleaving with haskell handle operations
prop_UnsafePayloadArrivesInOrder :: (Handle, Handle) -> BufferMode -> ByteString -> Property
prop_UnsafePayloadArrivesInOrder (p1, p2) bufMode payload = monadicIO $ do
    run (hSetBuffering p1 bufMode)
    let count = length payload
    run (withTempFile payload $ \fp -> do
             hPut p1 beg
             unsafeSendFile p1 fp
             hPut p1 end
             hFlush p1) -- flush after last put
    payload' <- run (hGet p2 (count + length beg + length end))
    assert ((beg `append` payload `append` end) == payload')
    where beg = (pack "BEGINNING")
          end = (pack "END")

prop_UnsafePartialPayloadArrives :: (Handle, Handle) -> BufferMode -> ByteString -> Property
prop_UnsafePartialPayloadArrives (p1, p2) bufMode payload = monadicIO $ do
    run (hSetBuffering p1 bufMode)
    count <- pick (choose (0, length payload))
    run (withTempFile payload $ \fp -> do
             unsafeSendFile' p1 fp 0 (fromIntegral count))
    payload' <- run (hGet p2 count) 
    assert (take count payload == payload')

prop_UnsafePartialPayloadWithOffsetArrives :: (Handle, Handle) -> BufferMode -> ByteString -> Property
prop_UnsafePartialPayloadWithOffsetArrives (p1, p2) bufMode payload = monadicIO $ do
    run (hSetBuffering p1 bufMode)
    let len = length payload
    offset <- pick (choose (0, len))
    let count = len - offset
    run (withTempFile payload $ \fp -> do
             unsafeSendFile' p1 fp (fromIntegral offset) (fromIntegral count))
    payload' <- run (hGet p2 count)
    assert (take count (drop offset payload) == payload')
{-
prop_UnsafeHandlePositionIgnored :: (Handle, Handle) -> BufferMode -> ByteString -> Property
prop_UnsafeHandlePositionIgnored (p1, p2) bufMode payload = monadicIO $ do
    run (hSetBuffering p1 bufMode)
    let len = length payload
    offset <- pick (choose (0, len))
    randpos <- pick (choose (0, len))
    let count = len - offset
    pre (offset /= randpos)
    run (withTempFile payload $ \fp -> do
         withBinaryFile fp ReadMode $ \fd -> do
             hSeek fd AbsoluteSeek (fromIntegral randpos) -- try to mess up the transmission
             unsafeSendFile' p1 fd (fromIntegral offset) (fromIntegral count))
    payload' <- run (hGet p2 count)
    assert (take count (drop offset payload) == payload')
-}
test_UnsafeLargeFileSizeArrives :: (Handle, Handle) -> BufferMode -> H.Assertion
test_UnsafeLargeFileSizeArrives (p1, p2) bufMode = do
    hSetBuffering p1 bufMode
    -- file is assumed to be 3gb, and is already created (use GenLargeFile.hs)
--    withBinaryFile "large.txt" ReadMode $ \h -> do
    largeLen <- withFile "large.txt" ReadMode $ hFileSize
    _ <- forkIO (unsafeSendFile' p1 "large.txt" 0 largeLen)
    receivedLen <- recvCountBytes p2 (fromIntegral largeLen)
    H.assertEqual "all bytes arrived" receivedLen (fromIntegral largeLen)
    where -- largeLen = 3 * 1024 * 1024 * 1024
          recvCountBytes _    0 = return 0
          recvCountBytes h len = do
              recvLen <- fmap length (hGetNonBlocking h 4194304)
              fmap (recvLen +) (recvCountBytes h (len - recvLen))

withTempFile :: ByteString -> (FilePath -> IO a) -> IO a
withTempFile payload fun = do
      fp <- bracket
              (openBinaryTempFile "tmp" "test.txt")
              (hClose . snd)
              (\(fp,fh) -> hPut fh payload >> return fp)
      fun fp `finally` removeFile fp
