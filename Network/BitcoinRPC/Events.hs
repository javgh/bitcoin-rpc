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
    , LSBCheckpoint(..)
    , determineNewTransactions
#endif
    ) where

import Control.Concurrent
import Control.Watchdog
import Data.Serialize
import GHC.Generics
import System.Posix.Process
import System.Posix.Signals

import qualified Data.Map as M

import Network.BitcoinRPC

-- | How deep in the block chain does a block need to be so that it
-- is considered final - i.e. will not change because of a re-org.
lsbBlockDepth :: Integer
lsbBlockDepth = 100

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

-- | Keep track on how we need to call 'listsinceblock' next to get
-- everything new that happened.
data LSBCheckpoint = LSBCheckpoint { lsbBlockHash :: Maybe BlockHash
                                   , lsbKnownTxIDs :: [TransactionID]
                                   }
                     deriving (Eq,Show,Read,Generic)

-- | Keeps track of confirmation count for recent unique transactions.
type UTXPool = M.Map UniqueTransactionID Integer

data EventTaskState = EventTaskState { etsLSBCheckpoint :: LSBCheckpoint
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

instance Serialize LSBCheckpoint

instance Serialize EventTaskState

initialEventTaskState :: EventTaskState
initialEventTaskState = EventTaskState { etsLSBCheckpoint =
                                            LSBCheckpoint Nothing []
                                       , etsPool = M.empty
                                       }

getUniqueTransactionID :: Transaction -> UniqueTransactionID
getUniqueTransactionID tx = UniqueTransactionID (tTxid tx) (tEntry tx)

onlyReceive :: Transaction -> Bool
onlyReceive ReceiveTx{} = True
onlyReceive _ = False

getNewBitcoinEvents :: Maybe WatchdogLogger-> RPCAuth-> (TransactionHeader -> Bool)-> EventTaskState-> IO (EventTaskState, [BitcoinEvent])
getNewBitcoinEvents mLogger auth acceptTest (EventTaskState lsbCheckpoint utxPool) = do
    (lsbCheckpoint', newTxs) <- getNewTransactions mLogger auth lsbCheckpoint
    let newReceiveTxs = filter onlyReceive newTxs
        newPoolEntries = map (\tx -> (getUniqueTransactionID tx, 0)) newReceiveTxs
        utxPool' = utxPool `M.union` M.fromList newPoolEntries
    appearingEvents <- mapM (augmentNewTransaction mLogger auth) newReceiveTxs
    (utxPool'', updateEvents) <- updatePool mLogger auth acceptTest utxPool'
    let newState = EventTaskState lsbCheckpoint' utxPool''
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

getNewTransactions :: Maybe WatchdogLogger -> RPCAuth -> LSBCheckpoint -> IO (LSBCheckpoint, [Transaction])
getNewTransactions mLogger auth lsbCheckpoint = do
    -- get (possibly) new transactions
    SinceBlockInfo txs _ <- listSinceBlockR mLogger auth (lsbBlockHash lsbCheckpoint)
    -- pick a block somewhat deep in the blockchain for the next query
    blockCount <- getBlockCountR mLogger auth
    let safeBlockHeight = max 0 (blockCount - lsbBlockDepth)
    safeBlockHash <- getBlockHashR mLogger auth safeBlockHeight
    let lsbCheckpoint' = lsbCheckpoint { lsbBlockHash = Just safeBlockHash }
    return $ determineNewTransactions lsbCheckpoint' txs

-- | Figure out which are actual new transactions and update
-- pool of known transaction ids. We simply keep the current list of
-- transactions returned by listsinceblock and check against it the next time.
-- This should work, as long as transactions never reappear after the have
-- dropped of the list. This should be true, as long as blockcount is always
-- increasing.
determineNewTransactions :: LSBCheckpoint -> [Transaction] -> (LSBCheckpoint, [Transaction])
determineNewTransactions (LSBCheckpoint mBlockHash knownTxIDs) txList =
    let newTransactions = filter (\tx -> tTxid tx `notElem` knownTxIDs) txList
        knownTxIDs' = map tTxid txList
    in ((LSBCheckpoint mBlockHash knownTxIDs'), newTransactions)

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
