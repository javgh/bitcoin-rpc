{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import System.Environment
import System.Exit

import Network.BitcoinRPC
import Network.BitcoinRPC.Events.MarkerAddresses

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

    let maConfig = [(BitcoinAddress "1MAbwuYp8CPChJ1ua25tnEKXkfXTVqEoyg", 1000000)]
        fetState = updateMarkerAddresses initialFilteredEventTaskState maConfig
    fbetHandle <- initFilteredBitcoinEventTask Nothing debugAuth pidfile
                                                    acceptTest fetState
    loop fetState fbetHandle

formatPendingTx :: Show a => (Transaction, a) -> String
formatPendingTx (tx, reason) =
    show (tAddress tx) ++ "; " ++ show (tAmount tx) ++ "; " ++ show reason

formatFilteredTx :: FilteredBitcoinEvent -> String
formatFilteredTx fTx@FilteredNewTransaction{} =
    "New transaction: " ++ show (tAddress . fntTx $ fTx)
        ++ "; " ++ show (tAmount . fntTx $ fTx)
formatFilteredTx MarkerAddressBreached{} = ""

loop :: FilteredEventTaskState -> FilteredBitcoinEventTaskHandle -> IO ()
loop fetState fbetHandle = do
    let pendingTxsOutput =
            unlines . map formatPendingTx $ listPendingTransactions fetState
    let maStatusOutput =
            unlines . map show $ listMarkerAdressStatus fetState
    writeFile "statusfile1" pendingTxsOutput
    writeFile "statusfile2" maStatusOutput
    (fetState', fEvents) <- waitForFilteredBitcoinEvents fbetHandle
    let filteredOutput = unlines . map formatFilteredTx $ fEvents
    putStr filteredOutput
    loop fetState' fbetHandle
