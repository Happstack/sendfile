import SendFile
import Network
import Network.Socket (ShutdownCmd(..), shutdown)
import System.IO

main = do
    service <- listenOn (PortNumber 8000)
    (clientHandle, _, _) <- accept service
    fileSize <- withFile "test.hs" ReadMode hFileSize
    hGetContents clientHandle
 
    putStrLn "received client request"
    
    hPutStr clientHandle "HTTP/1.1 200 OK\r\n"
    hPutStr clientHandle "Content-Type: text/plain\r\n"
    hPutStr clientHandle ("Content-Length: " ++ (show fileSize) ++ "\r\n")
    hPutStr clientHandle ("Connection: close\r\n")
    hPutStr clientHandle "\r\n"
    sendFile clientHandle "test.hs"
    
    putStrLn ("Response sent in mode: " ++ sendFileMode)
