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

massageData :: [Transaction] -> [Transaction] -> (LSBCheckpoint, [Transaction])
massageData txsA txsB =
    let allTxs = sortBy (comparing tTime) (txsA ++ txsB)
    in (LSBCheckpoint Nothing (map tTxid txsA), allTxs)

propNewTransactions :: [ArbTransaction] -> [ArbTransaction] -> Bool
propNewTransactions txsA txsB =
    let (txsA', txsB') = (map unATX txsA, map unATX txsB)
        (lsbCheckpoint, txs) = massageData txsA' txsB'
        knownTxIDs = lsbKnownTxIDs lsbCheckpoint
        (_, newTxs) = determineNewTransactions lsbCheckpoint txs
    in all (\tx -> tTxid tx `notElem` knownTxIDs) newTxs

eventsTests :: [Test]
eventsTests = [ testProperty "new transactions are returned" propNewTransactions
              ]
