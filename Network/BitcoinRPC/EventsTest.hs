module Network.BitcoinRPC.EventsTest
    ( eventsTests
    ) where

import Data.List
import Data.Ord
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Network.BitcoinRPC.Events
import Network.BitcoinRPC.TestTypes
import Network.BitcoinRPC.Types

massageData :: [Transaction] -> [Transaction] -> (LRSCheckpoint, [Transaction])
massageData txsA txsB =
    let allTxs = sortBy (comparing tTime) (txsA ++ txsB)
        oldestTimestamp = if null allTxs
                            then 0
                            else tTime (head allTxs)
    in (LRSCheckpoint oldestTimestamp (map tTxid txsA), allTxs)

propTimestampIncreases :: [ArbTransaction] -> [ArbTransaction] -> Bool
propTimestampIncreases txsA txsB =
    let (txsA', txsB') = (map unATX txsA, map unATX txsB)
        (lrsCheckpoint, txs) = massageData txsA' txsB'
        (lrsCheckpoint', _) = determineNewTransactions lrsCheckpoint txs
    in lrsTimestamp lrsCheckpoint <= lrsTimestamp lrsCheckpoint'

propNewTransactions :: [ArbTransaction] -> [ArbTransaction] -> Bool
propNewTransactions txsA txsB =
    let (txsA', txsB') = (map unATX txsA, map unATX txsB)
        (lrsCheckpoint, txs) = massageData txsA' txsB'
        knownTxIDs = lrsKnownTxIDs lrsCheckpoint
        (_, newTxs) = determineNewTransactions lrsCheckpoint txs
    in all (\tx -> tTxid tx `notElem` knownTxIDs) newTxs

eventsTests :: [Test]
eventsTests = [ testProperty "timestamp increases" propTimestampIncreases
              , testProperty "new transactions are returned" propNewTransactions
              ]
