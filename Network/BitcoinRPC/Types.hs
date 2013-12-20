{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Network.BitcoinRPC.Types
    ( BitcoinAmount(..)
    , BitcoinAddress(..)
    , TransactionID(..)
    , BlockHash(..)
    , RPCAuth(..)
    , Transaction(..)
    , TransactionHeader(..)
    , TransactionOrigins(..)
    , BitcoinAddressInfo(..)
    , SinceBlockInfo(..)
    , RPCResult(..)
    , SendError(..)
    , txidAsByteString
    , addressAsByteString
    , blockHashAsByteString
    )
    where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Serialize
import GHC.Generics

import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

newtype BitcoinAmount = BitcoinAmount { btcAmount :: Integer }
                        deriving (Eq,Ord,Show,Read,Generic)

newtype BitcoinAddress = BitcoinAddress { btcAddress :: T.Text }
                         deriving (Eq,Ord,Show,Read)

newtype TransactionID = TransactionID { btcTxID :: T.Text }
                        deriving (Eq,Ord,Show,Read)

newtype BlockHash = BlockHash { btcBlockHash :: T.Text }
                    deriving (Eq,Ord,Show,Read)

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
                 deriving (Eq,Show,Read,Generic)


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

data RPCResult = RPCSuccess Value
               | RPCError { rpcErrorCode :: Integer
                          , rpcErrorMessage :: T.Text
                          }
               deriving (Show)

data BitcoinAddressInfo = BitcoinAddressInfo { baiIsValid :: Bool
                                             , baiIsMine :: Bool
                                             }
                                             deriving (Show)

data SinceBlockInfo = SinceBlockInfo { sbiTransactions :: [Transaction]
                                     , sbiLastBlock :: BlockHash
                                     }
                                     deriving (Show)

data SendError = InvalidAddress | InsufficientFunds | InvalidAmount | OtherError
                deriving (Show)

instance Serialize BitcoinAmount

instance Serialize BitcoinAddress where
    put = put . T.unpack . btcAddress
    get = BitcoinAddress . T.pack <$> get

instance Serialize TransactionID where
    put = put . T.unpack . btcTxID
    get = TransactionID . T.pack <$> get

instance Serialize Transaction

instance Serialize BlockHash where
    put = put . T.unpack . btcBlockHash
    get = BlockHash . T.pack <$> get

instance Num BitcoinAmount
  where
    (+) (BitcoinAmount x) (BitcoinAmount y) = BitcoinAmount (x + y)
    (*) (BitcoinAmount _) (BitcoinAmount _) = error "not supported"
    (-) (BitcoinAmount x) (BitcoinAmount y) = BitcoinAmount (x - y)
    abs (BitcoinAmount x) = BitcoinAmount (abs x)
    signum (BitcoinAmount x) = BitcoinAmount (signum x)
    fromInteger x = BitcoinAmount (fromInteger x)

instance FromJSON BitcoinAmount
  where
    parseJSON v = do
        amount <- parseJSON v :: Parser Double
        let inSatoshis = round $ amount * 10 ^ (8 :: Integer)
        return $ BitcoinAmount inSatoshis

instance FromJSON BitcoinAddress
  where
    parseJSON (String addr) = return $ BitcoinAddress addr
    parseJSON _ = mzero

instance FromJSON TransactionID
  where
    parseJSON (String txid) = return $ TransactionID txid
    parseJSON _ = mzero

instance FromJSON BlockHash
  where
    parseJSON (String hash) = return $ BlockHash hash
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

instance FromJSON BitcoinAddressInfo
  where
    parseJSON (Object o) = BitcoinAddressInfo <$>
                                o .: "isvalid" <*>
                                (fromMaybe False <$> o .:? "ismine")
    parseJSON _ = mzero

instance FromJSON SinceBlockInfo
  where
    parseJSON (Object o) = SinceBlockInfo <$>
                            o .: "transactions" <*>
                            o .: "lastblock"
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

txidAsByteString :: TransactionID -> B.ByteString
txidAsByteString = E.encodeUtf8 . btcTxID

addressAsByteString :: BitcoinAddress -> B.ByteString
addressAsByteString = E.encodeUtf8 . btcAddress

blockHashAsByteString :: BlockHash -> B.ByteString
blockHashAsByteString = E.encodeUtf8 . btcBlockHash
