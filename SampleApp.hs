module Main where

import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.Environment
import System.Exit

import Network.BitcoinRPC
import Network.BitcoinRPC.Events

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
    forever $ do
        (_, events) <- readChan chan
        mapM print events
