{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.ZMQ3.Monadic
import ZHelpers (setRandomIdentity)
import Control.Concurrent (threadDelay)
import Data.ByteString.Char8 (pack, unpack)
import Control.Monad (forever, forM_, when, replicateM_)
import System.Random (newStdGen, randomR, StdGen)

clientTask :: String -> ZMQ z ()
clientTask ident = do
    client <- socket Dealer
    setRandomIdentity client
    connect client "tcp://localhost:5570"
    forM_ [1..] $ \i -> do -- forever
        -- tick one per second, pulling in arriving messages
        forM_ [0..100] $ \_ -> do
            [evts] <- poll 10 [Sock client [In] Nothing] -- timeout of 10 ms
            when (In `elem` evts) $
                receive client >>= \msg -> liftIO $ putStrLn $ unwords ["Client", ident, "has received back from worker its msg \"", (unpack msg), "\""]
        send client [] (pack $ unwords ["Client", ident, "sends request", show i])

            
serverTask :: ZMQ z ()
serverTask =  do
    frontend <- socket Router
    bind frontend "tcp://*:5570"
    backend <- socket Dealer
    bind backend "inproc://backend"
    
    replicateM_ 5 $ async serverWorker

    proxy frontend backend Nothing


serverWorker :: ZMQ z ()
serverWorker = do
    worker <- socket Dealer
    connect worker "inproc://backend"
    liftIO $ putStrLn "Worker Started"        
    forever $ do
        _id <- receive worker
        msg <- receive worker
        gen <- liftIO $ newStdGen
        let (val, gen') = randomR (0,4) gen :: (Int, StdGen)
            (r, _)      = randomR (1, 1000) gen' :: (Int, StdGen)
        -- send back the received msg to client 0 to 4 times max
        forM_ [0..val] $ \_ -> do
            liftIO $ threadDelay $ r * 1000
            send worker [SendMore] _id
            send worker [] msg

main :: IO ()
main = 
    runZMQ $ do
        async $ clientTask "A"
        async $ clientTask "B"
        async $ clientTask "C"
        async serverTask
        
        liftIO $ threadDelay $ 10 * 1000 * 1000