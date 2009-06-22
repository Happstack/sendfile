import SendFile
import Network
import System.IO

main = do
    service <- listenOn (PortNumber 8000)
    (clientHandle, _, _) <- accept service
    sendFile clientHandle "test.hs"
    putStrLn ("Sent in mode: " ++ sendFileMode)
    hClose clientHandle
    sClose service
