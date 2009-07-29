{-# OPTIONS_GHC -Wall #-}
module GenLargeFile where
import System.IO (Handle, IOMode(..), hFlush, hPutStr, hPutChar, stdout, withBinaryFile)
import System.Random (StdGen, getStdGen, randoms)

main :: IO ()
main = withBinaryFile "large.txt" WriteMode $ \h -> do
    putStrLn "Writing a 3 GB file to 'large.txt'"
    g <- getStdGen
    genContents' g h (3 * 1024 * 1024 * 1024)

-- works on windows, since hSetFileSize only works up to 2gb
genContents' :: StdGen -> Handle -> Integer -> IO ()
genContents' g h len
    | len <= 0  = return ()
    | otherwise = do
          hPutChar stdout '.'
          hFlush stdout
          let bytes = min len (128 * 1024)
          hPutStr h (take (fromIntegral bytes) (randoms g))
          genContents' g h (len - bytes)
