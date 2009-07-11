module SocketPair (prop_PairConnected, socketPair) where
import Control.Concurrent
import Control.Exception (bracket)
import Data.ByteString.Char8 (ByteString, hGet, hPut, length, pack)
import Network (PortID(..), Socket, accept, connectTo, listenOn, sClose, socketPort)
import Prelude hiding (length)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.IO (Handle, hFlush)

instance Arbitrary ByteString where
    arbitrary = fmap pack arbitrary

main = do
    pair <- socketPair
    quickCheck (prop_PairConnected pair)

accept' :: Socket -> IO Handle
accept' service = fmap (\(h,_,_) -> h) (accept service)

socketPair :: IO (Handle, Handle)
socketPair = bracket
    (listenOn (PortNumber 61234))
    (sClose)
    (\svc -> do port <- socketPort svc
                mp1 <- newEmptyMVar
                forkIO (accept' svc >>= putMVar mp1)
                p2 <- connectTo "127.0.0.1" port
                p1 <- takeMVar mp1
                return (p1, p2))

prop_PairConnected :: (Handle, Handle) -> ByteString -> Property
prop_PairConnected (p1, p2) payload = monadicIO $ do
    let len = length payload
    run $ hPut p1 payload
    run $ hFlush p1
    run $ hPut p2 payload
    run $ hFlush p2
    pre =<< fmap (== payload) (run $ hGet p1 len)
    pre =<< fmap (== payload) (run $ hGet p2 len)
