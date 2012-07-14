-- |
-- How to use:
--
-- 1. Use 'forkIO' (or similar) to start 'bitcoinEventTask' in its own thread.
--
-- 2. Listen on the channel for event updates. Every batch of updates
--    will also include a new state description, which can be passed to
--    'bitcoinEventTask' when it needs to be restarted.

{-# LANGUAGE OverloadedStrings, CPP #-}
module Network.BitcoinRPC.Events
    ( initialEventTaskState
    , bitcoinEventTask
    , LRSCheckpoint(..)
#if !PRODUCTION
    , determineNewTransactions
#endif
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad.Error(ErrorT)
import Control.Monad.Reader(ReaderT)
import Control.Watchdog
import System.Posix.Process
import System.Posix.Signals

import qualified Data.Map as M

import Network.BitcoinRPC

--numberOfConfirmationsNeeded :: Integer
--numberOfConfirmationsNeeded = 6

-- | Number of confirmations until we can be sure, that
-- the time field of a transaction does not change anymore.
lrsCheckpointConfirmations :: Integer
lrsCheckpointConfirmations = 100

-- | Approximate time it takes to accumlate the number of confirmations
-- specified in 'lrsCheckpointConfirmations'.
lrsCheckpointInterval :: Integer
lrsCheckpointInterval = lrsCheckpointConfirmations * 10 * 60

-- | Create identifier that can differentiate between the outgoing and incoming
-- side of the same transaction id.  Slightly confusing as this means that a
-- single Bitcoin transaction can result in multiple unique transactions.
data UniqueTransactionID = UniqueTransactionID { uTxID :: TransactionID
                                               , uEntry :: Integer
                                               }
                           deriving (Eq, Show, Read)

-- | Keep track on how we need to call 'listreceivedsince' next to get
-- everything new that happened.
data LRSCheckpoint = LRSCheckpoint { lrsTimestamp :: Integer
                                   , lrsKnownTxIDs :: [TransactionID]
                                   }
                     deriving (Eq,Show,Read)

-- | Keeps track of confirmation count for recent unique transactions.
type UTXPool = M.Map UniqueTransactionID Integer

data EventTaskState = EventTaskState { etsLRSCheckpoint :: LRSCheckpoint
                                     , etsPool :: UTXPool
                                     }

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

--getEventIdentifier :: Transaction -> EventIdentifier
--getEventIdentifier tx = EventIdentifier (tTxid tx) (tEntry tx)
--
--getNewTransactionEvents :: TransactionEventState -> ErrorT String (ReaderT RPCConfiguration IO) (TransactionEventState, [EventUpdate])
--getNewTransactionEvents (TransactionEventState checkpoint pool) = do
--    (checkpoint', newTxs) <- getNewTransactions checkpoint
--    let newReceiveTxs = filter onlyReceive newTxs
--        pool' = pool ++ map (\tx -> (getEventIdentifier tx, 0)) newReceiveTxs
--    appearingEvents <- mapM augmentNewTransaction newReceiveTxs
--    (pool'', updateEvents) <- updatePool pool'
--    let poolFiltered = filter checkPoolRequirement pool''
--        newState = TransactionEventState checkpoint' poolFiltered
--    return (newState, appearingEvents ++ updateEvents)
--
--augmentNewTransaction :: Transaction -> ErrorT String (ReaderT RPCConfiguration IO) (EventUpdate)
--augmentNewTransaction tx = do
--    let txid = tTxid tx
--        eventIdentifier = getEventIdentifier tx
--    probe <- doRPCRequestFiltered errorCodeInvalidTransactionID (mkGetOriginsCmd txid)
--    case probe of
--        Just txOrigins -> return $ (eventIdentifier, NewTransaction (TransactionWithOrigin tx (toOrigins txOrigins)))
--        Nothing -> return $ (eventIdentifier, NewTransaction (TransactionWithOrigin tx []))
--
--onlyReceive :: Transaction -> Bool
--onlyReceive (ReceiveTx _ _ _ _ _ _) = True
--onlyReceive _ = False
--
---- | Remove transactions from the pool that have either
----   been completely confirmed or have disappeared.
--checkPoolRequirement :: (t, Integer) -> Bool
--checkPoolRequirement (_, count) = count >= 0 && count <= numberOfConfirmationsNeeded
--
--updatePool :: [EventConfirmationCount] -> ErrorT String (ReaderT RPCConfiguration IO) ([EventConfirmationCount], [EventUpdate])
--    updates <- mapM updatePoolEntry pool
--    let pool' = map fst updates
--        updateEvents = concatMap snd updates
--    return (pool', updateEvents)
--  where
--    updatePoolEntry (eventIdentifier@(EventIdentifier txid _), confirmationCount) = do
--        -- note: this will sometimes make redundant calls to gettransaction,
--        --       but it's easier to implement so I leave it for now
--        probe <- doRPCRequestFiltered errorCodeInvalidTransactionID (mkGetTransactionCmd txid)
--        case probe of
--            Just updatedTx -> do
--                let confirmationCount' = thConfirmations updatedTx
--                    events = createIntermediateEvents eventIdentifier confirmationCount confirmationCount'
--                return ((eventIdentifier, confirmationCount'), map markAcceptedTransactions events)
--            Nothing -> return ((eventIdentifier, -1), [(eventIdentifier, TransactionDisappeared)])
--
--createIntermediateEvents :: t -> Integer -> Integer -> [(t, TransactionEvent)]
--createIntermediateEvents txid start stop =
--    let stop' = min stop numberOfConfirmationsNeeded
--    in if start < stop'
--        then map (\cC -> (txid, TransactionUpdate cC)) $ tail [start..stop']
--        else []
--
--markAcceptedTransactions :: (t, TransactionEvent) -> (t, TransactionEvent)
--markAcceptedTransactions event@(eventIdentifier, TransactionUpdate cC)
--    | cC == numberOfConfirmationsNeeded = (eventIdentifier, TransactionAccepted)
--    | otherwise = event
--markAcceptedTransactions event = event
--
--getNewTransactions :: EventCheckpoint-> BitcoinMonadStack (EventCheckpoint, [Transaction])
--getNewTransactions checkpoint@(EventCheckpoint timestamp _) = do
--    request <- doRPCRequest $ mkListReceivedSinceCmd timestamp
--    return $ processTransactions checkpoint (assertAscending request)
--
--processTransactions :: EventCheckpoint-> [Transaction]-> (EventCheckpoint, [Transaction])
--processTransactions checkpoint [] = (checkpoint, [])
--processTransactions (EventCheckpoint _ knownTxIDs) txList =
--    let newTransactions = filter (\tx -> tTxid tx `notElem` knownTxIDs) txList
--        youngTxs = filter (\tx -> tConfirmations tx < eventCheckPointConfirmations) txList
--        lastTimestamp = tTime (last txList)
--        edgeTxs = filter (\tx -> tTime tx == lastTimestamp) txList   -- edgeTxs can never be empty
--    in if null youngTxs
--            then (EventCheckpoint lastTimestamp (map tTxid edgeTxs), newTransactions)
--            else let firstTimestamp = tTime (head youngTxs)
--                 in (EventCheckpoint firstTimestamp (map tTxid youngTxs), newTransactions)
--
--assertAscending :: [Transaction] -> [Transaction]
--assertAscending [] = []
--assertAscending txList =
--    let times = map tTime txList
--        pairs = zip times (tail times)
--        isAscending = all (uncurry (<=)) pairs
--    in assert isAscending txList
--

initialEventTaskState :: EventTaskState
initialEventTaskState = EventTaskState { etsLRSCheckpoint =
                                            LRSCheckpoint 0 []
                                       , etsPool = M.empty
                                       }

getNewBitcoinEvents mLogger auth (EventTaskState lrsCheckpoint utxPool) = do
    (lrsCheckpoint', newTxs) <- getNewTransactions mLogger auth lrsCheckpoint
    print lrsCheckpoint'
    print newTxs
    return (undefined, undefined)

getNewTransactions :: Maybe WatchdogLogger -> RPCAuth -> LRSCheckpoint -> IO (LRSCheckpoint, [Transaction])
getNewTransactions mLogger auth lrsCheckpoint = do
    txs <- listReceivedSinceR mLogger auth (lrsTimestamp lrsCheckpoint)
    let ascendingTxs = assertAscending txs
    return $ determineNewTransactions lrsCheckpoint ascendingTxs
  where

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
                        then tTime (last youngTxs) - lrsCheckpointConfirmations
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

--notifiedPollLoop :: RPCConfiguration -> TransactionEventState -> Chan (TransactionEventState, [EventUpdate]) -> IO b
notifiedPollLoop semaphore mLogger auth firstState chan = go firstState
  where
    go state = do
        _ <- takeMVar semaphore     -- Will succeed right away the first time,
                                    -- then later only after signals have been
                                    -- received.
        (state', events) <- getNewBitcoinEvents mLogger auth state
        writeChan chan (state', events)
        go state'

--bitcoinEventTask :: RPCConfiguration
--                    -> FilePath     -- ^ PID will be written here to get notifications form Bitcoin daemon
--                    -> TransactionEventState -- ^ state from which to resume
--                    -> Chan (TransactionEventState, [EventUpdate]) -- ^ channel which will receive updates
--                    -> IO b
--bitcoinEventTask connectionConf pidfile state chan = do
--    setupBitcoinNotifcation pidfile
--    notifiedPollLoop connectionConf state chan

bitcoinEventTask mLogger auth pidfile state chan = do
    semaphore <- setupBitcoinNotifcation pidfile
    notifiedPollLoop semaphore mLogger auth state chan
