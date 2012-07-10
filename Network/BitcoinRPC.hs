{-# LANGUAGE OverloadedStrings #-}
module Network.BitcoinRPC
    ( errorCodeInvalidTransactionID
    , getBlockCountR
    , module Network.BitcoinRPC.Types
    )
    where

import Control.Applicative
import Control.Monad
import Control.Watchdog
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Network.Browser
import Network.HTTP
import Network.URI

import qualified Data.ByteString.Char8 as B8
import qualified Control.Exception as E
import qualified Data.Attoparsec as AP
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

import Network.BitcoinRPC.Types

errorCodeInvalidTransactionID :: Integer
errorCodeInvalidTransactionID = -5

debugAuth = RPCAuth "http://127.0.0.1:8332" "rpcuser" "localaccessonly"

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
callApi :: RPCAuth-> B8.ByteString -> B8.ByteString -> IO (Either String Value)
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
callApiFiltered :: RPCAuth-> B8.ByteString-> B8.ByteString-> Integer-> IO (Either String (Maybe Value))
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


-- | Make a call to the Bitcoin daemon. 'Left' is returned when either
--   a network error occured or the response is not valid JSON or the JSON
--   structure is not in the expected format.
callApiHelper :: RPCAuth -> B8.ByteString -> B8.ByteString -> IO (Either String RPCResult)
callApiHelper auth method params = do
    let (+++) = B8.append
        cmdStr =
            "{ \"id\": 0 " +++ ", \"method\": \"" +++ method +++ "\" " +++
                               ", \"params\": " +++ params +++ " " +++
            "}"
        requestPayload = compileRequest uri cmdStr
    result <- ioTry . browse $ do
        setOutHandler $ const (return ())
        addAuthority authority
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
    authority = AuthBasic { auRealm = "jsonrpc"
                          , auUsername = rpcUser auth
                          , auPassword = rpcPassword auth
                          , auSite = uri
                          }
    uri = fromMaybe (error "RPC-URL is malformed") $ parseURI (rpcUrl auth)
    jsonParse = AP.parseOnly json

compileRequest :: URI -> B8.ByteString -> Request B8.ByteString
compileRequest uri body =
    Request { rqURI = uri
            , rqMethod = POST
            , rqHeaders = [ Header HdrContentType "application/json"
                          , Header HdrContentLength (show . B8.length $ body)
                          ]
            , rqBody = body
            }

getBlockCountR :: Maybe WatchdogLogger -> RPCAuth -> IO Integer
getBlockCountR mLogger auth = do
    v <- reliableApiCall mLogger $ callApi auth "getblockcount" "[]"
    let p :: Result Integer
        p = fromJSON v
    case p of
        Success r -> return r
        Error _ -> error "Unexpected result when calling method getblockcount"
