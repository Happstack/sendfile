module Network.Socket.SendFile.Iter where

import Control.Concurrent (threadWaitWrite)
import Data.Int (Int64)
import System.Posix.Types (Fd)

data Iter 
    = Sent       Int64 (IO Iter)    -- ^ number of bytes sent this pass and a continuation to send more
    | WouldBlock Int64 Fd (IO Iter) -- ^ number of bytes sent, Fd that blocked, continuationto send more. NOTE: The Fd should not be used outside the running of the Iter as it may be freed when the Iter is done.
    | Done       Int64              -- ^ number of bytes sent, no more to send.

runIter :: IO Iter -> IO Int64
runIter = runIter' 0
    where
      runIter' :: Int64 -> IO Iter -> IO Int64
      runIter' acc iter =
          do r <- iter
             case r of
               (Sent n cont) -> 
                   do let acc' = (acc + n) 
--                      putStrLn $ "Sent " ++ show acc'
                      acc' `seq` runIter' acc' cont
               (Done n) ->
                   do -- putStrLn $ "Done " ++ show (acc + n)
                      return (acc + n)
               (WouldBlock n fd cont) -> 
                   do threadWaitWrite fd
                      let acc' = (acc + n) 
--                      putStrLn $ "WouldBlock " ++ (show acc')
                      acc' `seq` runIter' acc' cont
