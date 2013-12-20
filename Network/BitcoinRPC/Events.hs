{-# LANGUAGE DeriveGeneric #-}
-- |
-- How to use:
--
-- 1. Use 'initBitcoinEventTask' to start a thread which will listen for Bitcoin
--    events.
--
-- 2. Use 'waitForBitcoinEvents' to listen for event updates. Every batch of
--    updates will also include a new state description, which can be passed to
--    'initBitcoinEventTask' when it needs to be restarted.
--
-- Note: For every new transaction you are guaranteed to receive a
-- 'NewTransaction' as well as either a 'TransactionAccepted' or
-- 'TransactionDisappeared'. Usually you will also receive 'TransactionUpdate'
-- when the number of confirmations change, but these events might not be
-- generated if you are catching up from an old state.

{-# LANGUAGE OverloadedStrings, CPP #-}
module Network.BitcoinRPC.Events
    ( initialEventTaskState
    , initBitcoinEventTask
    , waitForBitcoinEvents
    , killBitcoinEventTask
    , BitcoinEvent(..)
    , UniqueTransactionID(..)
    , EventTaskState
    , BitcoinEventTaskHandle
#if !PRODUCTION
    , LRSCheckpoint(..)
    , determineNewTransactions
#endif
    ) where

import Control.Concurrent
import Control.Exception
import Control.Watchdog
import Data.Serialize
import GHC.Generics
import System.Posix.Process
import System.Posix.Signals

import qualified Data.Map as M

import Network.BitcoinRPC

-- | Number of confirmations until we can be sure, that
-- the time field of a transaction does not change anymore.
lrsCheckpointConfirmations :: Integer
lrsCheckpointConfirmations = 100

-- | Approximate time it takes to accumlate the number of confirmations
-- specified in 'lrsCheckpointConfirmations'.
lrsCheckpointInterval :: Integer
lrsCheckpointInterval = lrsCheckpointConfirmations * 10 * 60

data BitcoinEventTaskHandle = BitcoinEventTaskHandle
                                { bethChan ::
                                    Chan (EventTaskState, [BitcoinEvent])
                                , bethThreadId :: ThreadId
                                }

-- | Create identifier that can differentiate between the outgoing and incoming
-- side of the same transaction id.  Slightly confusing as this means that a
-- single Bitcoin transaction can result in multiple unique transactions.
data UniqueTransactionID = UniqueTransactionID { uTxID :: TransactionID
                                               , uEntry :: Integer
                                               }
                           deriving (Eq, Ord, Show, Read, Generic)

-- | Keep track on how we need to call 'listreceivedsince' next to get
-- everything new that happened.
data LRSCheckpoint = LRSCheckpoint { lrsTimestamp :: Integer
                                   , lrsKnownTxIDs :: [TransactionID]
                                   }
                     deriving (Eq,Show,Read,Generic)

-- | Keeps track of confirmation count for recent unique transactions.
type UTXPool = M.Map UniqueTransactionID Integer

data EventTaskState = EventTaskState { etsLRSCheckpoint :: LRSCheckpoint
                                     , etsPool :: UTXPool
                                     }
                      deriving (Show,Generic)

data BitcoinEvent = NewTransaction { beUTxID :: UniqueTransactionID
                                   , beTx :: Transaction
                                   , beOrigins :: [BitcoinAddress]
                                   }
                  | TransactionUpdate { beUTxID :: UniqueTransactionID
                                      , beConfirmations :: Integer
                                      }
                  | TransactionAccepted { beUTxID :: UniqueTransactionID }
                  | TransactionDisappeared { beUTxID :: UniqueTransactionID }
                  deriving (Show)

instance Serialize UniqueTransactionID

instance Serialize LRSCheckpoint

instance Serialize EventTaskState

initialEventTaskState :: EventTaskState
initialEventTaskState = EventTaskState { etsLRSCheckpoint =
                                            LRSCheckpoint 0 []
                                       , etsPool = M.empty
                                       }

getUniqueTransactionID :: Transaction -> UniqueTransactionID
getUniqueTransactionID tx = UniqueTransactionID (tTxid tx) (tEntry tx)

onlyReceive :: Transaction -> Bool
onlyReceive ReceiveTx{} = True
onlyReceive _ = False

getNewBitcoinEvents :: Maybe WatchdogLogger-> RPCAuth-> (TransactionHeader -> Bool)-> EventTaskState-> IO (EventTaskState, [BitcoinEvent])
getNewBitcoinEvents mLogger auth acceptTest (EventTaskState lrsCheckpoint utxPool) = do
    (lrsCheckpoint', newTxs) <- getNewTransactions mLogger auth lrsCheckpoint
    let newReceiveTxs = filter onlyReceive newTxs
        newPoolEntries = map (\tx -> (getUniqueTransactionID tx, 0)) newReceiveTxs
        utxPool' = utxPool `M.union` M.fromList newPoolEntries
    appearingEvents <- mapM (augmentNewTransaction mLogger auth) newReceiveTxs
    (utxPool'', updateEvents) <- updatePool mLogger auth acceptTest utxPool'
    let newState = EventTaskState lrsCheckpoint' utxPool''
    return (newState, appearingEvents ++ updateEvents)

updatePool :: Maybe WatchdogLogger-> RPCAuth-> (TransactionHeader -> Bool)-> M.Map UniqueTransactionID Integer-> IO (M.Map UniqueTransactionID Integer, [BitcoinEvent])
updatePool mLogger auth acceptTest utxPool = go utxPool (M.toList utxPool)
  where
    go pool [] = return (pool, [])
    go pool ((utxid, confs):entries) = do
        -- Note: This will sometimes make redundant calls to gettransaction,
        --       but keeps things a little simpler.
        probe <- getTransactionR mLogger auth (uTxID utxid)
        let (pool', events) = checkUpdate acceptTest pool utxid confs probe
        (pool'', moreEvents) <- go pool' entries
        return (pool'', events ++ moreEvents)

checkUpdate :: (TransactionHeader -> Bool)-> M.Map UniqueTransactionID Integer-> UniqueTransactionID-> Integer-> Maybe TransactionHeader-> (M.Map UniqueTransactionID Integer, [BitcoinEvent])
checkUpdate acceptTest pool utxid confs (Just updatedTxHeader)
    | thConfirmations updatedTxHeader == confs = (pool, [])
    | otherwise = if acceptTest updatedTxHeader
                    then (M.delete utxid pool, [TransactionAccepted utxid])
                    else let updatedConfs = thConfirmations updatedTxHeader
                         in (M.insert utxid updatedConfs pool,
                                [TransactionUpdate utxid updatedConfs])
checkUpdate _ pool utxid _ Nothing =
    (M.delete utxid pool, [TransactionDisappeared utxid])

augmentNewTransaction :: Maybe WatchdogLogger -> RPCAuth -> Transaction -> IO BitcoinEvent
augmentNewTransaction mLogger auth tx = do
    let txid = tTxid tx
        utxid = getUniqueTransactionID tx
    probe <- getOriginsR mLogger auth txid
    return $ case probe of
        Just txOrigins -> NewTransaction utxid tx (toOrigins txOrigins)
        Nothing -> NewTransaction utxid tx []

getNewTransactions :: Maybe WatchdogLogger -> RPCAuth -> LRSCheckpoint -> IO (LRSCheckpoint, [Transaction])
getNewTransactions mLogger auth lrsCheckpoint = do
    txs <- listReceivedSinceR mLogger auth (lrsTimestamp lrsCheckpoint)
    let ascendingTxs = assertAscending txs
    return $ determineNewTransactions lrsCheckpoint ascendingTxs

-- | Figure out which are actual new transactions and decide on a new
-- checkpoint. Since timestamps of recent transactions can still change, the
-- checkpoint needs to be some time in the past. Also, transactions might be
-- lingering unconfirmed for a long time, so the checkpoint needs to be at least
-- as far in the past as these unconfirmed transactions.
determineNewTransactions :: LRSCheckpoint -> [Transaction]-> (LRSCheckpoint, [Transaction])
determineNewTransactions (LRSCheckpoint timestamp _) [] =
    (LRSCheckpoint timestamp [], [])
determineNewTransactions (LRSCheckpoint timestamp knownTxIDs) txList =
    let newTransactions = filter (\tx -> tTxid tx `notElem` knownTxIDs) txList
        youngTxs = filter (\tx -> tConfirmations tx < lrsCheckpointConfirmations) txList
        -- Select a new possible checkpoint. Difficulty: We do not know what
        -- time is 'now', we can only estimate it from the available
        -- transactions.
        -- We would like to pick a checkpoint which is:
        --   a) not earlier than the old checkpoint
        --   b) at least some time in the past compared to 'now'
        --   c) includes all lingering (< 100 confs) transactions, so we can
        --      get updates on them on future listreceivedsince calls
        --   d) not unnecessary far into the past to keep knownTxIDs small
        candidateD = tTime (last txList) + 1
        safeCandidate = candidateD
        candidateC = if not (null youngTxs)
                        then tTime (head youngTxs)
                        else safeCandidate
        candidateB = if not (null youngTxs)
                        then tTime (last youngTxs) - lrsCheckpointInterval
                        else safeCandidate
        candidateBCD = (min (min candidateB candidateC) candidateD) -- earliest
        newTimestamp = max timestamp candidateBCD   -- not earlier than old one
    in (LRSCheckpoint newTimestamp (map tTxid txList), newTransactions)

assertAscending :: [Transaction] -> [Transaction]
assertAscending [] = []
assertAscending txList =
    let times = map tTime txList
        pairs = zip times (tail times)
        isAscending = all (uncurry (<=)) pairs
    in assert isAscending txList

setupBitcoinNotifcation :: FilePath -> IO (MVar ())
setupBitcoinNotifcation path = do
    pid <- getProcessID
    writeFile path (show pid ++ "\n")
    installSignalHandlers

installSignalHandlers :: IO (MVar ())
installSignalHandlers = do
    semaphore <- newMVar ()
    _ <- installHandler userDefinedSignal1 (Catch $ signalHandler semaphore) Nothing
    _ <- installHandler userDefinedSignal2 (Catch $ signalHandler semaphore) Nothing
    return semaphore
  where
    signalHandler semaphore = putMVar semaphore ()

notifiedPollLoop :: MVar ()-> Maybe WatchdogLogger-> RPCAuth-> (TransactionHeader -> Bool)-> EventTaskState-> Chan (EventTaskState, [BitcoinEvent])-> IO ()
notifiedPollLoop semaphore mLogger auth acceptTest firstState chan = go firstState
  where
    go state = do
        _ <- takeMVar semaphore     -- Will succeed right away the first time,
                                    -- then later only after signals have been
                                    -- received.
        (state', events) <- getNewBitcoinEvents mLogger auth acceptTest state
        writeChan chan (state', events)
        go state'

initBitcoinEventTask :: Maybe WatchdogLogger
                     -> RPCAuth
                     -> FilePath    -- ^  PID will be written here to get
                                    -- notifications from Bitcoin daemon
                     -> (TransactionHeader -> Bool)
                                    -- ^ test to decide whether a transaction
                                    -- can be accepted
                     -> EventTaskState  -- ^ state from which to resume
                     -> IO (BitcoinEventTaskHandle)
initBitcoinEventTask mLogger auth pidfile acceptTest state = do
    chan <- newChan
    semaphore <- setupBitcoinNotifcation pidfile
    threadId <-
        forkIO $ notifiedPollLoop semaphore mLogger auth acceptTest state chan
    return $ BitcoinEventTaskHandle chan threadId

-- | Wait for new Bitcoin Events. This might sometimes return an empty list of
-- events.
waitForBitcoinEvents :: BitcoinEventTaskHandle -> IO (EventTaskState, [BitcoinEvent])
waitForBitcoinEvents betHandle = readChan (bethChan betHandle)

killBitcoinEventTask :: BitcoinEventTaskHandle -> IO ()
killBitcoinEventTask betHandle = killThread (bethThreadId betHandle)

listReceivedSinceR = undefined
