import SendFile
import Network
import System.IO

main = do
    service <- listenOn (PortNumber 8000)
    (clientHandle, _, _) <- accept service
    fileSize <- withFile "test.hs" ReadMode hFileSize
    hPutStr clientHandle "HTTP/1.1 200 OK\r\n"
    hPutStr clientHandle "Content-Type: text/plain\r\n"
    hPutStr clientHandle ("Content-Length: " ++ (show fileSize) ++ "\r\n\r\n")

    sendFile clientHandle "test.hs"
    putStrLn ("Sent in mode: " ++ sendFileMode)
    hClose clientHandle
    sClose service
