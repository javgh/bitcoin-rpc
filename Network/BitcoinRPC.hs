{-# LANGUAGE OverloadedStrings #-}
module Network.BitcoinRPC
    ( getBlockCountR
    , getBlockHashR
    , listSinceBlockR
    , getTransactionR
    , getRawTransactionR
    , getOriginsR
    , getNewAddressR
    , getBalanceR
    , validateAddressR
    , sendToAddress
    , module Network.BitcoinRPC.Types
    , debugAuth
    , debugAddr
    , debugAmount
    )
    where

import Control.Applicative
import Control.Watchdog
import Data.Aeson
import Data.Maybe
import Network.Browser
import Network.HTTP
import Network.URI
import Text.Printf

import qualified Control.Exception as E
import qualified Data.Attoparsec as AP
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import Network.BitcoinRPC.Types

errorCodeInvalidTransactionID :: Integer
errorCodeInvalidTransactionID = -5

errorCodeInvalidAddress :: Integer
errorCodeInvalidAddress = -5

errorCodeInsufficientFunds :: Integer
errorCodeInsufficientFunds = -4

errorCodeInvalidAmount :: Integer
errorCodeInvalidAmount = -3

debugAuth :: RPCAuth
debugAuth = RPCAuth "http://127.0.0.1:8332" "rpcuser" "localaccessonly"

debugAddr :: BitcoinAddress
debugAddr = BitcoinAddress "19LT33L5fp1eUQK5uJjMMGAgiyrqMjX5ii"

debugAmount :: BitcoinAmount
debugAmount = BitcoinAmount (10 * 10 ^ (8 :: Integer))

ioTry :: IO a -> IO (Either E.IOException a)
ioTry = E.try

reliableApiCall :: Maybe WatchdogLogger -> IO (Either String a) -> IO a
reliableApiCall mLogger f = watchdog $ do
    case mLogger of
        Just logger -> setLoggingAction logger
        Nothing -> return ()
    watch f

-- | Make a call to the Bitcoin daemon, expecting that there will
--   be no RPC error. Network and other parse errors are signaled by a 'Left'
callApi :: RPCAuth-> B.ByteString -> B.ByteString -> IO (Either String Value)
callApi auth method params = do
    result <- callApiHelper auth method params
    return $ case result of
        Left e -> Left e
        Right (RPCSuccess v) -> Right v
        Right (RPCError {}) ->
            error $ "Unexpected RPC error when calling method "
                    ++ B8.unpack method ++ ": " ++ show result

-- | Make a call to the Bitcoin daemon, expecting that there will
--   be only one specific type of RPC error possible. Network and
--   other parse errors are signaled by a 'Left', whereas the occurence
--   of that specific error code is signaled by 'Right Nothing'.
callApiFiltered :: RPCAuth-> B.ByteString-> B.ByteString-> Integer-> IO (Either String (Maybe Value))
callApiFiltered auth method params conceivableError = do
    result <- callApiHelper auth method params
    return $ case result of
        Left e -> Left e
        Right (RPCSuccess v) -> Right (Just v)
        Right (RPCError { rpcErrorCode = errCode }) ->
            if errCode == conceivableError
                then Right Nothing
                else error $ "Unexpected RPC error when calling method "
                     ++ B8.unpack method ++ ": " ++ show result

-- | Make a call to the Bitcoin daemon and report all types of RPC errors.
-- Network and other parse errors are signaled by a 'Left', whereas the
-- occurence of RPC errors are signaled by 'Right Left errorCode'.
callApiCarefully :: RPCAuth-> B8.ByteString-> B8.ByteString-> IO (Either String (Either Integer Value))
callApiCarefully auth method params = do
    result <- callApiHelper auth method params
    return $ case result of
        Left e -> Left e
        Right (RPCSuccess v) -> Right (Right v)
        Right (RPCError { rpcErrorCode = errCode }) -> Right (Left errCode)

-- | Make a call to the Bitcoin daemon. 'Left' is returned when either
--   a network error occured or the response is not valid JSON or the JSON
--   structure is not in the expected format.
callApiHelper :: RPCAuth -> B.ByteString -> B.ByteString -> IO (Either String RPCResult)
callApiHelper auth method params = do
    let (+++) = B.append
        cmdStr =
            "{ \"id\": 0 " +++ ", \"method\": \"" +++ method +++ "\" " +++
                               ", \"params\": " +++ params +++ " " +++
            "}"
        requestPayload = compileRequest uri cmdStr
    result <- ioTry . browse $ do
        setOutHandler $ const (return ())
        addAuthority rpcAuthority
        setAllowBasicAuth True
        request requestPayload
    return $ case result of
        Left e -> Left $ "IOException: " ++ show e
        Right (_, response) ->
            case jsonParse (rspBody response) of
                Left e' -> Left $ "JSON parse error: " ++ e'
                Right v -> case fromJSON v of
                    (Error e'') -> Left $ "RPC parse error: " ++ e''
                    (Success s) -> Right (s :: RPCResult)
  where
    rpcAuthority = AuthBasic { auRealm = "jsonrpc"
                             , auUsername = rpcUser auth
                             , auPassword = rpcPassword auth
                             , auSite = uri
                             }
    uri = fromMaybe (error "RPC-URL is malformed") $ parseURI (rpcUrl auth)
    jsonParse = AP.parseOnly json

compileRequest :: URI -> B.ByteString -> Request B.ByteString
compileRequest uri body =
    Request { rqURI = uri
            , rqMethod = POST
            , rqHeaders = [ Header HdrContentType "application/json"
                          , Header HdrContentLength (show . B.length $ body)
                          ]
            , rqBody = body
            }

parseReply :: (Monad m, FromJSON a) => String -> Value -> m a
parseReply method v =
    case fromJSON v of
        Success r -> return r
        Error _ -> error ("Unexpected result when calling method " ++ method)

getBlockCountR :: Maybe WatchdogLogger -> RPCAuth -> IO Integer
getBlockCountR mLogger auth = do
    v <- reliableApiCall mLogger $ callApi auth "getblockcount" "[]"
    parseReply "getblockcount" v :: IO Integer

getBlockHashR :: Maybe WatchdogLogger -> RPCAuth -> Integer -> IO BlockHash
getBlockHashR mLogger auth idx = do
    let params = "[" `B.append` (B8.pack . show) idx `B.append` "]"
    v <- reliableApiCall mLogger $ callApi auth "getblockhash" params
    parseReply "getblockhash" v :: IO BlockHash

listSinceBlockR :: Maybe WatchdogLogger-> RPCAuth -> Maybe BlockHash -> IO SinceBlockInfo
listSinceBlockR mLogger auth mBlockHash = do
    let params = case mBlockHash of
                        Nothing -> "[]"
                        Just hash -> "[\"" `B.append` blockHashAsByteString hash
                                        `B.append` "\"]"
    v <- reliableApiCall mLogger $ callApi auth "listsinceblock" params
    parseReply "listsinceblock" v :: IO SinceBlockInfo

getTransactionR :: Maybe WatchdogLogger-> RPCAuth -> TransactionID -> IO (Maybe TransactionHeader)
getTransactionR mLogger auth txid = do
    let params = "[\"" `B.append` txidAsByteString txid `B.append` "\"]"
        conceivableError = errorCodeInvalidTransactionID
    v <- reliableApiCall mLogger $
            callApiFiltered auth "gettransaction" params conceivableError
    case v of
        Just v' -> Just <$> parseReply "gettransaction" v'
                                :: IO (Maybe TransactionHeader)
        Nothing -> return Nothing

getRawTransactionR :: Maybe WatchdogLogger-> RPCAuth -> TransactionID -> IO (Maybe T.Text)
getRawTransactionR mLogger auth txid = do
    let params = "[\"" `B.append` txidAsByteString txid `B.append` "\"]"
        conceivableError = errorCodeInvalidTransactionID
    v <- reliableApiCall mLogger $
            callApiFiltered auth "getrawtransaction" params conceivableError
    case v of
        Just v' -> Just <$> parseReply "getrawtransaction" v'
                                :: IO (Maybe T.Text)
        Nothing -> return Nothing

getOriginsR :: Maybe WatchdogLogger-> RPCAuth -> TransactionID -> IO (Maybe TransactionOrigins)
getOriginsR mLogger auth txid = do
    let params = "[\"" `B.append` txidAsByteString txid `B.append` "\"]"
        conceivableError = errorCodeInvalidTransactionID
    v <- reliableApiCall mLogger $
            callApiFiltered auth "getorigins" params conceivableError
    case v of
        Just v' -> Just <$> parseReply "getorigins" v'
                                :: IO (Maybe TransactionOrigins)
        Nothing -> return Nothing

getNewAddressR :: Maybe WatchdogLogger -> RPCAuth -> IO BitcoinAddress
getNewAddressR mLogger auth = do
    v <- reliableApiCall mLogger $ callApi auth "getnewaddress" "[]"
    parseReply "getnewaddress" v :: IO BitcoinAddress

getBalanceR :: Maybe WatchdogLogger -> RPCAuth -> Integer -> IO BitcoinAmount
getBalanceR mLogger auth minconf = do
    let params = "[\"*\", " `B.append` (B8.pack . show) minconf  `B.append` "]"
    v <- reliableApiCall mLogger $ callApi auth "getbalance" params
    parseReply "getbalance" v :: IO BitcoinAmount

validateAddressR :: Maybe WatchdogLogger-> RPCAuth -> BitcoinAddress -> IO BitcoinAddressInfo
validateAddressR mLogger auth addr = do
    let params = "[\"" `B.append` addressAsByteString addr `B.append` "\"]"
    v <- reliableApiCall mLogger $ callApi auth "validateaddress" params
    parseReply "validateaddress" v :: IO BitcoinAddressInfo

-- | Send Bitcoins to an address. This is not available in a reliable
-- version, because of the risk of ending up in some type of loop and sending
-- funds multiple types. Instead, network and parse errors are signaled with
-- a 'Left' whereas a successful call will produce a 'Right' which contains
-- another 'Either'. This one differentiates between 'SendError' and a
-- successful transaction resulting in a 'TransactionID'.
--
-- It is recommended to call a reliable function like 'validateAddressR' shortly
-- before attempting to use 'sendToAddress'. The former will only return when a
-- connection to the Bitcoin daemon exists, therefore minimizing the risk that
-- 'sendToAddress' will encounter any network problems afterwards.
sendToAddress :: RPCAuth-> BitcoinAddress-> BitcoinAmount-> IO (Either String (Either SendError TransactionID))
sendToAddress auth addr amount = do
    let intAmount = btcAmount amount
        doubleAmount = fromInteger intAmount / 10 ^ (8 :: Integer)
        strAmount = printf "%.8f" (doubleAmount :: Double)
        params = "[\"" `B.append` addressAsByteString addr `B.append`
                      "\"," `B.append` B8.pack strAmount `B.append` "]"
    v <- callApiCarefully auth "sendtoaddress" params
    case v of
        Left e -> return $ Left e
        Right (Left errCode) -> return $ Right (Left (translateError errCode))
        Right (Right v') -> do
            r <- parseReply "sendtoaddress" v' :: IO TransactionID
            return $ Right (Right r)
  where
    translateError code
        | code == errorCodeInvalidAddress = InvalidAddress
        | code == errorCodeInsufficientFunds = InsufficientFunds
        | code == errorCodeInvalidAmount = InvalidAmount
        | otherwise = OtherError
