{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Monad
import System.Environment
import System.Exit

import Network.BitcoinRPC
import Network.BitcoinRPC.Events
import Network.BitcoinRPC.MarkerAddresses

acceptTest :: TransactionHeader -> Bool
acceptTest txHeader = thConfirmations txHeader >= 3

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    when (null args) $ do
        putStrLn $ "Usage: " ++ progName ++ " <path to notify pid file>"
        exitFailure
    let pidfile = head args

    chan <- newChan
    _ <- forkIO $ bitcoinEventTask Nothing debugAuth pidfile acceptTest
                        initialEventTaskState chan
    let maStore = initMarkerAddressStore
                        [(BitcoinAddress "1MAbwuYp8CPChJ1ua25tnEKXkfXTVqEoyg", 0)]
    loop maStore chan

loop maStore chan = do
    (_, events) <- readChan chan
    putStrLn "--- store before calling processEvents:"
    print maStore
    putStrLn "--- events:"
    print events
    let (maStore', fEvents) = processEvents maStore events
    putStrLn  "--- store after calling processEvents:"
    print maStore'
    putStrLn "--- filtered events:"
    print fEvents
    putStrLn ""
    loop maStore' chan
