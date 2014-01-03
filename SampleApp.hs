module Main where

import Control.Monad
import System.Environment
import System.Exit

import Network.BitcoinRPC
import Network.BitcoinRPC.Events

acceptTest :: TransactionHeader -> Bool
acceptTest txHeader = thConfirmations txHeader >= 3

rpcAuth :: RPCAuth
rpcAuth = RPCAuth "http://127.0.0.1:8332" "rpcuser" "localaccessonly"

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    when (null args) $ do
        putStrLn $ "Usage: " ++ progName ++ " <path to notify pid file>"
        exitFailure
    let pidfile = head args

    betHandle <- initBitcoinEventTask Nothing rpcAuth pidfile
                                        acceptTest initialEventTaskState
    forever $ do
        (_, events) <- waitForBitcoinEvents betHandle
        mapM print events
