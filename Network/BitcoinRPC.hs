{-# LANGUAGE OverloadedStrings #-}

module Network.BitcoinRPC
    ( errorCodeInvalidTransactionID
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

errorCodeInvalidTransactionID :: Integer
errorCodeInvalidTransactionID = -5

newtype BitcoinAmount = BitcoinAmount { btcAmount :: Integer }
                        deriving (Eq,Show,Read)

newtype BitcoinAddress = BitcoinAddress { btcAddress :: T.Text }
                         deriving (Eq,Show,Read)

newtype TransactionID = TransactionID { btcTxID :: T.Text }
                        deriving (Eq,Show,Read)

data RPCAuth = RPCAuth { rpcUrl :: String
                       , rpcUser :: String
                       , rpcPassword :: String
                       }
                       deriving (Show)

data Transaction = ReceiveTx { tEntry :: Integer
                             , tAmount :: BitcoinAmount
                             , tAddress :: BitcoinAddress
                             , tConfirmations :: Integer
                             , tTxid :: TransactionID
                             , tTime :: Integer
                             }
                 | SendTx { tEntry :: Integer
                          , tAmount :: BitcoinAmount
                          , tAddress :: BitcoinAddress
                          , tFee :: BitcoinAmount
                          , tConfirmations :: Integer
                          , tTxid :: TransactionID
                          , tTime :: Integer
                          }
                 | MoveTx { tTime :: Integer }
                 | GenerateTx { tEntry :: Integer
                              , tAmount :: BitcoinAmount
                              , tConfirmations :: Integer
                              , tTxid :: TransactionID
                              , tTime :: Integer
                              }
                 deriving (Eq,Show,Read)


data TransactionHeader = TransactionHeader { thAmount :: BitcoinAmount
                                           , thConfirmations :: Integer
                                           , thTxid :: TransactionID
                                           , thTime :: Integer
                                           }
                         deriving (Show)

data TransactionOrigins = TransactionOrigins { toConfirmations :: Integer
                                             , toTxid :: TransactionID
                                             , toTime :: Integer
                                             , toOrigins :: [BitcoinAddress]
                                             }
                          deriving (Show)

data BlockCount = BlockCount { bcCount :: Integer }
                  deriving (Show)

data RPCResult = RPCSuccess Value
               | RPCError { rpcErrorCode :: Integer
                          , rpcErrorMessage :: T.Text
                          }
               deriving (Show)

instance Num BitcoinAmount
  where
    (+) (BitcoinAmount x) (BitcoinAmount y) = BitcoinAmount (x + y)
    (*) (BitcoinAmount x) (BitcoinAmount y) = error "not supported"
    (-) (BitcoinAmount x) (BitcoinAmount y) = BitcoinAmount (x - y)
    abs (BitcoinAmount x) = BitcoinAmount (abs x)
    signum (BitcoinAmount x) = BitcoinAmount (signum x)
    fromInteger x = BitcoinAmount (fromInteger x)

instance FromJSON BitcoinAmount
  where
    parseJSON v = do
        amount <- parseJSON v :: Parser Double
        let inSatoshis = round $ amount * 10^8
        return $ BitcoinAmount inSatoshis

instance FromJSON BitcoinAddress
  where
    parseJSON (String addr) = return $ BitcoinAddress addr
    parseJSON _ = mzero

instance FromJSON TransactionID
  where
    parseJSON (String txid) = return $ TransactionID txid
    parseJSON _ = mzero

instance FromJSON Transaction
  where
    parseJSON (Object o) = case H.lookup "category" o of
        Just "send" -> SendTx <$>
                            o .: "entry" <*>
                            (abs <$> o .: "amount") <*>
                            o .: "address" <*>
                            o .: "fee" <*>
                            o .: "confirmations" <*>
                            o .: "txid" <*>
                            o .: "time"
        Just "receive" -> ReceiveTx <$>
                            o .: "entry" <*>
                            o .: "amount" <*>
                            o .: "address" <*>
                            o .: "confirmations" <*>
                            o .: "txid" <*>
                            o .: "time"
        Just "move" -> MoveTx <$>
                            o .: "time"
        Just "generate" -> GenerateTx <$>
                            o .: "entry" <*>
                            o .: "amount" <*>
                            o .: "confirmations" <*>
                            o .: "txid" <*>
                            o .: "time"
        Just _ -> mzero
        Nothing -> mzero
    parseJSON _ = mzero

instance FromJSON TransactionHeader
  where
    parseJSON (Object o) = TransactionHeader <$>
                            o .: "amount" <*>
                            o .: "confirmations" <*>
                            o .: "txid" <*>
                            o .: "time"
    parseJSON _ = mzero

instance FromJSON TransactionOrigins
  where
    parseJSON (Object o) = TransactionOrigins <$>
                            o .: "confirmations" <*>
                            o .: "txid" <*>
                            o .: "time" <*>
                            o .: "origins"
    parseJSON _ = mzero

instance FromJSON BlockCount
  where
    parseJSON v@(Number _) = BlockCount <$> parseJSON v
    parseJSON _ = mzero

instance FromJSON RPCResult
  where
    parseJSON (Object o) = case H.lookup "result" o of
        Just Null -> case H.lookup "error" o of
            Just (Object errInfo) -> RPCError <$> errInfo .: "code"
                                              <*> errInfo .: "message"
            Just Null -> mzero
            _ -> mzero
        Just result -> return $ RPCSuccess result
        _ -> mzero
    parseJSON _ = mzero

reliableApiCall :: IO (Either String a) -> IO a
reliableApiCall f = watchdog $ watch f

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

ioTry :: IO a -> IO (Either E.IOException a)
ioTry = E.try

debugAuth = RPCAuth "http://127.0.0.1:8332" "rpcuser" "localaccessonly"
