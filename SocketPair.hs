module SocketPair (prop_HandlePairConnected, prop_SocketPairConnected, handlePair, recvAll, socketPair) where
import Control.Concurrent
import Control.Exception (bracket)
import Data.ByteString.Char8 (ByteString, append, empty, hGet, hPut, length, pack)
import Network (PortID(..), Socket, listenOn, sClose)
import Network.Socket (Family(..), SockAddr(..), SocketType(..), accept, aNY_PORT, connect, defaultProtocol, getSocketName, inet_addr, socket, socketPort, socketToHandle)
import Network.Socket.ByteString (recv, sendAll)
import Prelude hiding (length)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.IO (Handle, IOMode(..), hFlush)

instance Arbitrary ByteString where
    arbitrary = fmap pack arbitrary

main = do
    pair <- socketPair
    quickCheck . prop_SocketPairConnected =<< socketPair
    quickCheck . prop_HandlePairConnected =<< handlePair

accept' :: Socket -> IO Socket
accept' service = fmap (\(s,_) -> s) (accept service)

socketPair :: IO (Socket, Socket)
socketPair = bracket
    (listenOn (PortNumber aNY_PORT))
    (sClose)
    (\svc -> do port <- socketPort svc
                mp1 <- newEmptyMVar
                forkIO (accept' svc >>= putMVar mp1)
                p2 <- socket AF_INET Stream defaultProtocol
                port <- socketPort svc
                addr <- inet_addr "127.0.0.1"
                connect p2 (SockAddrInet port addr)
                p1 <- takeMVar mp1
                return (p1, p2))

handlePair :: IO (Handle, Handle)
handlePair = do
    (s1, s2) <- socketPair
    h1 <- socketToHandle s1 ReadWriteMode
    h2 <- socketToHandle s2 ReadWriteMode
    return (h1, h2)

prop_SocketPairConnected :: (Socket, Socket) -> ByteString -> Property
prop_SocketPairConnected (p1, p2) payload = monadicIO $ do
    let len = length payload
    pre (len > 0)
    run $ sendAll p1 payload
    run $ sendAll p2 payload
    pre =<< fmap (== payload) (run $ recvAll p1 len)
    pre =<< fmap (== payload) (run $ recvAll p2 len)

prop_HandlePairConnected :: (Handle, Handle) -> ByteString -> Property
prop_HandlePairConnected (p1, p2) payload = monadicIO $ do
    let len = length payload
    run $ hPut p1 payload
    run $ hPut p2 payload
    run $ hFlush p1
    run $ hFlush p2
    pre =<< fmap (== payload) (run $ hGet p1 len)
    pre =<< fmap (== payload) (run $ hGet p2 len)

-- like sendAll
recvAll :: Socket -> Int -> IO ByteString
recvAll _ 0 = return empty
recvAll sock len = do
    recv_dta <- recv sock len
    let recv_len = length recv_dta
    fmap (append recv_dta) (recvAll sock (len - recv_len))
