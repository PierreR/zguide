{-# LANGUAGE OverloadedStrings #-}
-- |
-- Weather broadcast server in Haskell
-- Binds SUB socket to tcp://localhost:5556
-- Collects weather updates and finds avg temp in zipcode

module Main where

import Control.Applicative((<$>), (<*>))
import qualified Control.Foldl as L
import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (mapMaybe)
import Text.Printf
import Safe (readMay)
import System.ZMQ4.Monadic

zipCode :: BC.ByteString
zipCode = "10001" -- NYC

-- traverse the list only once thanks to `Control.Foldl` applicatives.
average :: L.Fold Double Double
average = (/) <$> L.sum <*> L.genericLength

-- premap is part of Foldl since 1.0.2
premap :: (a -> b) -> L.Fold b r -> L.Fold a r
premap f (L.Fold step begin done) = L.Fold step' begin done
  where
    step' x = step x . f

main :: IO ()
main =
    runZMQ $ do
        subscriber <- socket Sub
        connect subscriber "ipc://weather.ipc"
        subscribe subscriber zipCode

        -- process 100 updates
        records <- replicateM 50 $ do
            update <- receive subscriber
            let [_, temp, hum] = mapMaybe readMay $ words (BC.unpack update)
            return (temp, hum)
        liftIO $ printf "NY City: avg temp of %.1f°C and avg hum of %.1f%%\n" (avgTemp records) (avgHum records)

        where
           avgTemp = L.fold (premap fst average)
           avgHum  = L.fold (premap snd average)
