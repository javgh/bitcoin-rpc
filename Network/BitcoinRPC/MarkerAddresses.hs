{-# LANGUAGE CPP, OverloadedStrings #-}
module Network.BitcoinRPC.MarkerAddresses
    ( initMarkerAddressStore
    , processEvents
    , listPendingTransactions
    , listMarkerAdressStatus
    , MAStore
    , PendingReason(..)
    , FilteredBitcoinEvent(..)
#if !PRODUCTION
    , sumAcceptedMarkerAmounts
#endif
    ) where

import Data.Function
import Data.List
import Data.Maybe
import Data.Ord

import qualified Data.Map as M

import Network.BitcoinRPC
import Network.BitcoinRPC.Events

data FilteredBitcoinEvent = FilteredNewTransaction { fntTx :: Transaction
                                                   , fntConfs :: Integer
                                                   , fntMarkerAddress ::
                                                        Maybe BitcoinAddress
                                                   }
                          | MarkerAddressBreached { fntTx :: Transaction
                                                  , fntBreachedMarkerAddress ::
                                                        BitcoinAddress
                                                  }
                          deriving (Show)

data PendingReason = TooFewConfirmations { prConfs :: Integer }
                   | MarkerAddressLimitReached { prMarkerAddress :: BitcoinAddress }
                   deriving (Show)

data MarkerAddressDetails = MarkerAddressDetails { madActive :: Bool
                                                 , madLimit :: BitcoinAmount
                                                 , madPendingAmount :: BitcoinAmount
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

initMarkerAddressStore :: [(BitcoinAddress, BitcoinAmount)] -> MAStore
initMarkerAddressStore markerAddresses =
    let confList = map transform markerAddresses
    in MAStore (M.fromList confList) M.empty
  where
    transform (addr, limit) = (addr, MarkerAddressDetails True limit 0)

listMarkerAdressStatus :: MAStore -> [(BitcoinAddress, Bool, BitcoinAmount, BitcoinAmount)]
listMarkerAdressStatus store = map format $ M.toList (masConf store)
  where
    format (ma, MarkerAddressDetails active limit pendingAmount) =
        (ma, active, limit, pendingAmount)

listPendingTransactions :: MAStore -> [(Transaction, PendingReason)]
listPendingTransactions store =
    let pendingTransactions = concatMap format $ M.toList (masPending store)
    in sortBy (comparing (tTime . fst)) pendingTransactions
  where
    format (_, PendingTransaction tx confs status) =
        case status of
            StandardTransaction -> [(tx, TooFewConfirmations confs)]
            PendingMarkerTransaction ma -> [(tx, MarkerAddressLimitReached ma)]
            AcceptedMarkerTransaction _ -> []

processEvents :: MAStore -> [BitcoinEvent] -> (MAStore, [FilteredBitcoinEvent])
processEvents store events =
    let (store', fEventsA) = foldl' updateStore (store, []) events
        amountList = sumAcceptedMarkerAmounts . listAcceptedMarkerAmounts $ store'
        store'' = setPendingMarkerAmounts store' amountList
        (store''', fEventsB) = checkPendingMarkerTransactions store''
    in (store''', fEventsA ++ fEventsB)

checkPendingMarkerTransactions :: MAStore -> (MAStore, [FilteredBitcoinEvent])
checkPendingMarkerTransactions store =
    let txs = M.toList (masPending store)
    in foldl' checkPendingMarkerTransaction (store, []) txs

checkPendingMarkerTransaction :: (MAStore, [FilteredBitcoinEvent])-> (UniqueTransactionID, PendingTransaction)-> (MAStore, [FilteredBitcoinEvent])
checkPendingMarkerTransaction (store, fevents) (utxid, pendingTransaction) =
    case ptStatus pendingTransaction of
        StandardTransaction -> (store, fevents)
        AcceptedMarkerTransaction _ -> (store, fevents)
        PendingMarkerTransaction ma ->
            let tx = ptTx pendingTransaction
                amount = tAmount tx
            in if isWithinLimit store ma amount
                  then let store' = adjustPendingAmount store ma (+ amount)
                           pendingTransaction' =
                              pendingTransaction { ptStatus =
                                  AcceptedMarkerTransaction ma }
                           masPending' = M.insert utxid pendingTransaction'
                                          (masPending store')
                           confs = ptConfs pendingTransaction
                           fEvent = FilteredNewTransaction tx confs (Just ma)
                       in (store' { masPending = masPending' }, fevents ++ [fEvent])
                  else (store, fevents)

isWithinLimit :: MAStore -> BitcoinAddress -> BitcoinAmount -> Bool
isWithinLimit store markerAddress amount =
    case M.lookup markerAddress (masConf store) of
        Nothing -> False
        Just details ->
            madActive details && madPendingAmount details + amount <= madLimit details

setPendingMarkerAmounts :: MAStore -> [(BitcoinAddress, BitcoinAmount)] -> MAStore
setPendingMarkerAmounts = foldl' go
  where
    go store (ma, amount) = adjustPendingAmount store ma (\_ -> amount)

adjustPendingAmount :: MAStore-> BitcoinAddress -> (BitcoinAmount -> BitcoinAmount) -> MAStore
adjustPendingAmount store ma f =
    let mDetails = M.lookup ma (masConf store)
    in case mDetails of
        Nothing -> store
        Just details ->
            let pendingAmount = madPendingAmount details
                details' = details { madPendingAmount = f pendingAmount }
                masConf' = M.insert ma details' (masConf store)
            in store { masConf = masConf' }

listAcceptedMarkerAmounts :: MAStore -> [(BitcoinAddress, BitcoinAmount)]
listAcceptedMarkerAmounts store =
    let zeroes = map (\(ma, _) -> (ma, 0)) $ M.toList (masConf store)
        amounts = concatMap extract $ M.toList (masPending store)
    in zeroes ++ amounts
  where
    extract (_, pendingTransaction) =
        case ptStatus pendingTransaction of
            StandardTransaction -> []
            PendingMarkerTransaction _ -> []
            AcceptedMarkerTransaction ma ->
                [(ma, tAmount (ptTx pendingTransaction))]

sumAcceptedMarkerAmounts :: [(BitcoinAddress, BitcoinAmount)] -> [(BitcoinAddress, BitcoinAmount)]
sumAcceptedMarkerAmounts =
    map (foldl1' sumAmounts) .  filter (not . null) .
        groupBy ((==) `on` fst) . sortBy (comparing fst)
  where
    sumAmounts :: (BitcoinAddress, BitcoinAmount) -> (BitcoinAddress, BitcoinAmount) -> (BitcoinAddress, BitcoinAmount)
    sumAmounts (ma1, x) (ma2, y) =
        if ma1 == ma2
            then (ma1, x + y)
            else error ("Implementation error: Expected to be summing"
                       ++ " data for the same marker address.")

updateStore :: (MAStore, [FilteredBitcoinEvent])-> BitcoinEvent -> (MAStore, [FilteredBitcoinEvent])
updateStore (store, fEvents) (NewTransaction utxid tx origins) =
    let pendingTransaction =
            case isFromMarkerAddress (masConf store) origins of
                Nothing -> PendingTransaction tx 0 StandardTransaction
                Just origin -> PendingTransaction tx 0
                                    (PendingMarkerTransaction origin)
        masPending' = M.insert utxid pendingTransaction (masPending store)
    in (store { masPending = masPending' }, fEvents)
updateStore (store, fEvents) (TransactionUpdate utxid confs) =
    let pendingTransaction = lookupPendingTransaction store utxid
        pendingTransaction' = pendingTransaction { ptConfs = confs }
        masPending' = M.insert utxid pendingTransaction' (masPending store)
    in (store { masPending = masPending' }, fEvents)
updateStore (store, fEvents) (TransactionAccepted utxid) =
    let pendingTransaction = lookupPendingTransaction store utxid
        tx = ptTx pendingTransaction
        confs = ptConfs pendingTransaction
        masPending' = M.delete utxid (masPending store)
        maybeFEvents = case ptStatus pendingTransaction of
                        StandardTransaction ->
                            [FilteredNewTransaction tx confs Nothing]
                        PendingMarkerTransaction psMA ->
                            [FilteredNewTransaction tx confs (Just psMA)]
                        AcceptedMarkerTransaction _ ->
                            [] -- has already been returned earlier
    in (store { masPending = masPending' }, fEvents ++ maybeFEvents)
updateStore (store, fEvents) (TransactionDisappeared utxid) =
    let pendingTransaction = lookupPendingTransaction store utxid
        (store', maybeFEvents) =
            case ptStatus pendingTransaction of
                    StandardTransaction -> (store, [])
                    PendingMarkerTransaction address ->
                        ( disableMarkerAddress store address
                        , [MarkerAddressBreached
                            (ptTx pendingTransaction) address]
                        )
                    AcceptedMarkerTransaction address ->
                        ( disableMarkerAddress store address
                        , [MarkerAddressBreached
                            (ptTx pendingTransaction) address]
                        )
        masPending' = M.delete utxid (masPending store')
    in (store' { masPending = masPending' }, maybeFEvents)

disableMarkerAddress :: MAStore -> BitcoinAddress -> MAStore
disableMarkerAddress store address =
    let details = lookupMarkerAddressDetails store address
        details' = details { madActive = False }
        masConf' = M.insert address details' (masConf store)
    in store { masConf = masConf' }

lookupMarkerAddressDetails :: MAStore -> BitcoinAddress -> MarkerAddressDetails
lookupMarkerAddressDetails store address =
    fromMaybe (error ("Inconsistency: Failed to lookup details"
                     ++ " for a marker address that is in use."))
              (M.lookup address (masConf store))

lookupPendingTransaction :: MAStore -> UniqueTransactionID -> PendingTransaction
lookupPendingTransaction store utxid =
    fromMaybe (error ("Inconsistency: Received an update for a"
                     ++ " transaction that I have not seen before."))
              (M.lookup utxid (masPending store))

isFromMarkerAddress :: Ord a => M.Map a b -> [a] -> Maybe a
isFromMarkerAddress conf origins = go origins
  where
    go [] = Nothing
    go (origin:origins) = if M.member origin conf
                            then Just origin
                            else go origins
