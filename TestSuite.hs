module Main where

import System.Exit
import Test.HUnit

import Network.BitcoinRPC

auth = RPCAuth "http://127.0.0.1:8332" "rpcuser" "localaccessonly"

test1 = TestLabel "Test for getblockcount" $ TestCase $ do
    _ <- getBlockCountR Nothing auth
    return ()

tests = TestList [test1]

main = do
    counts <- runTestTT tests
    if errors counts > 0 || failures counts > 0
        then exitFailure
        else exitSuccess
