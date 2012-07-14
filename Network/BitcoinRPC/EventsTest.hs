module Network.BitcoinRPC.EventsTest
    ( eventsTests
    ) where

import Control.Applicative
import Data.List
import Data.Ord
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Data.Text as T

import Network.BitcoinRPC.Events
import Network.BitcoinRPC.Types

-- Define newtype wrappers to avoid the 'orphan instance' warning.
newtype ArbBitcoinAddress = ABAddr { unABAddr :: BitcoinAddress }
                            deriving (Show)

newtype ArbTransactionID = ATI { unATI :: TransactionID }
                           deriving (Show)

newtype ArbBitcoinAmount = ABAmount { unABAmount :: BitcoinAmount }
                           deriving (Show)

newtype ArbTransaction = ATX { unATX :: Transaction }
                         deriving (Show)

instance Arbitrary ArbBitcoinAddress where
    arbitrary = do
        str <- suchThat arbitrary (\s -> length s > 0) :: Gen String
        return $ ABAddr (BitcoinAddress (T.pack str))

instance Arbitrary ArbTransactionID where
    arbitrary = do
        str <- suchThat arbitrary (\s -> length s > 0) :: Gen String
        return $ ATI (TransactionID (T.pack str))

instance Arbitrary ArbBitcoinAmount where
    arbitrary = do
        int <- choose (0,10 * 10 ^ (8::Integer))
        return $ ABAmount (BitcoinAmount int)

instance Arbitrary ArbTransaction where
    arbitrary = do
        entry <- choose (0, 2)
        amount <- unABAmount <$> arbitrary
        address <- unABAddr <$> arbitrary
        confs <- choose (0,200)
        txid <- unATI <$> arbitrary
        time <- choose (0, 1000000000)
        return $ ATX (ReceiveTx entry amount address confs txid time)

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
