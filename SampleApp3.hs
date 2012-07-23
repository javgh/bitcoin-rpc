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
                        [(BitcoinAddress "1MAbwuYp8CPChJ1ua25tnEKXkfXTVqEoyg", 1000000)]
    loop maStore chan

formatPendingTx :: Show a => (Transaction, a) -> [Char]
formatPendingTx (tx, reason) =
    show (tAddress tx) ++ "; " ++ show (tAmount tx) ++ "; " ++ show reason

loop maStore chan = do
    let pendingTxsOutput =
            unlines . map formatPendingTx $ listPendingTransactions maStore
    let maStatusOutput =
            unlines . map show $ listMarkerAdressStatus maStore
    writeFile "statusfile1" pendingTxsOutput
    writeFile "statusfile2" maStatusOutput
    (_, events) <- readChan chan
    putStrLn "--- events:"
    print events
    let (maStore', fEvents) = processEvents maStore events
    putStrLn "--- filtered events:"
    print fEvents
    putStrLn ""
    loop maStore' chan
