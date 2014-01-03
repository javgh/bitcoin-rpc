module Main where

import Network.BitcoinRPC

rpcAuth :: RPCAuth
rpcAuth = RPCAuth "http://127.0.0.1:8332" "rpcuser" "localaccessonly"

main :: IO ()
main = getBlockCountR Nothing rpcAuth >>= print
