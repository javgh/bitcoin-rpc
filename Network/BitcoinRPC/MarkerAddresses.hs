module Network.BitcoinRPC.MarkerAddresses
    ( initMarkerAddressStore
    , processEvents
    ) where

import Data.List
import Data.Maybe

import qualified Data.Map as M

import Network.BitcoinRPC
import Network.BitcoinRPC.Events

data FilteredBitcoinEvent = FilteredNewTransaction { fntTx :: Transaction
                                                   , fntConfs :: Integer
                                                   , fntMarkerAddress ::
                                                        Maybe BitcoinAddress
                                                   }
                                                   deriving (Show)

data MarkerAddressDetails = MarkerAddressDetails { madActive :: Bool
                                                 , madLimit :: Integer
                                                 , madPendingAmount :: Integer
                                                 }
                            deriving (Show)

type MarkerAddressesConf = M.Map BitcoinAddress MarkerAddressDetails

data PendingStatus = StandardTransaction
                   | PendingMarkerTransaction { psMarkerAddress :: BitcoinAddress }
                   | AcceptedMarkerTransaction { psMarkerAddress :: BitcoinAddress }
                   deriving (Show)

data PendingTransaction = PendingTransaction { ptTx :: Transaction
                                             , ptConfs :: Integer
                                             , ptStatus :: PendingStatus
                                             }
                          deriving (Show)

type PendingTransactions = M.Map UniqueTransactionID PendingTransaction

data MAStore = MAStore { masConf :: MarkerAddressesConf
                       , masPending :: PendingTransactions
                       }
                       deriving (Show)

initMarkerAddressStore :: [(BitcoinAddress, Integer)] -> MAStore
initMarkerAddressStore markerAddresses =
    let confList = map transform markerAddresses
    in MAStore (M.fromList confList) (M.empty)
  where
    transform (addr, limit) = (addr, MarkerAddressDetails True limit 0)

-- TODO: process list of PendingMarkerTransactionS

processEvents :: MAStore -> [BitcoinEvent] -> (MAStore, [FilteredBitcoinEvent])
processEvents store = foldl' updateStore (store, [])

-- TODO: write HUnit tests for updateStore

updateStore :: (MAStore, [FilteredBitcoinEvent])-> BitcoinEvent -> (MAStore, [FilteredBitcoinEvent])
updateStore (store, maEvents) (NewTransaction utxid tx origins) =
    let pendingTransaction =
            case isFromMarkerAddress (masConf store) origins of
                Nothing -> PendingTransaction tx 0 StandardTransaction
                Just origin -> PendingTransaction tx 0
                                    (PendingMarkerTransaction origin)
        masPending' = M.insert utxid pendingTransaction (masPending store)
    in (store { masPending = masPending' }, maEvents)
updateStore (store, maEvents) (TransactionUpdate utxid confs) =
    let pendingTransaction = lookupPendingTransaction store utxid
        pendingTransaction' = pendingTransaction { ptConfs = confs }
        masPending' = M.insert utxid pendingTransaction' (masPending store)
    in (store { masPending = masPending' }, maEvents)
updateStore (store, maEvents) (TransactionAccepted utxid) =
    let pendingTransaction = lookupPendingTransaction store utxid
        tx = ptTx pendingTransaction
        confs = ptConfs pendingTransaction
        ma = case ptStatus pendingTransaction of
                StandardTransaction -> Nothing
                PendingMarkerTransaction psMA -> Just psMA
                AcceptedMarkerTransaction psMA -> Just psMA
        masPending' = M.delete utxid (masPending store)
        fEvent = FilteredNewTransaction tx confs ma
    in (store { masPending = masPending' }, maEvents ++ [fEvent])
updateStore (store, maEvents) (TransactionDisappeared utxid) =
    let pendingTransaction = lookupPendingTransaction store utxid
        store' = case ptStatus pendingTransaction of
                    StandardTransaction -> store
                    PendingMarkerTransaction address ->
                        disableMarkerAddress store address
                    AcceptedMarkerTransaction address ->
                        disableMarkerAddress store address
        masPending' = M.delete utxid (masPending store')
    in (store' { masPending = masPending' }, [])

disableMarkerAddress :: MAStore -> BitcoinAddress -> MAStore
disableMarkerAddress store address =
    let details = lookupMarkerAddressDetails store address
        details' = details { madActive = False }
        masConf' = M.insert address details' (masConf store)
    in store { masConf = masConf' }

lookupMarkerAddressDetails :: MAStore -> BitcoinAddress -> MarkerAddressDetails
lookupMarkerAddressDetails store address =
    fromMaybe (error "Inconsistency: Failed to lookup details\
                     \ for a marker address that is in use.")
              (M.lookup address (masConf store))

lookupPendingTransaction :: MAStore -> UniqueTransactionID -> PendingTransaction
lookupPendingTransaction store utxid =
    fromMaybe (error "Inconsistency: Received an update for a\
               \ transaction that I have not seen before.")
              (M.lookup utxid (masPending store))

isFromMarkerAddress :: Ord a => M.Map a b -> [a] -> Maybe a
isFromMarkerAddress conf origins = go origins
  where
    go [] = Nothing
    go (origin:origins) = if M.member origin conf
                            then Just origin
                            else go origins
