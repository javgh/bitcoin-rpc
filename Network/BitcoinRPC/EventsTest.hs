module EventsTests
    (
    ) where

import Control.Applicative
import Data.List
import Data.Ord
import Test.QuickCheck

import qualified Data.Text as T

import Network.BitcoinRPC.Events
import Network.BitcoinRPC.Types

instance Arbitrary BitcoinAddress where
    arbitrary = do
        str <- suchThat arbitrary (\s -> length s > 0) :: Gen String
        return $ BitcoinAddress (T.pack str)

instance Arbitrary TransactionID where
    arbitrary = do
        str <- suchThat arbitrary (\s -> length s > 0) :: Gen String
        return $ TransactionID (T.pack str)

instance Arbitrary BitcoinAmount where
    arbitrary = do
        int <- choose (0,10 * 10 ^ (8::Integer))
        return $ BitcoinAmount int

instance Arbitrary Transaction where
    arbitrary = ReceiveTx <$> choose (0,2)
                          <*> arbitrary
                          <*> arbitrary
                          <*> choose (0,200)
                          <*> arbitrary
                          <*> choose (0, 1000000000)

massageData :: [Transaction] -> [Transaction] -> (LRSCheckpoint, [Transaction])
massageData txsA txsB =
    let allTxs = sortBy (comparing tTime) (txsA ++ txsB)
        oldestTimestamp = if null allTxs
                            then 0
                            else tTime (head allTxs)
    in (LRSCheckpoint oldestTimestamp (map tTxid txsA), allTxs)

propTimestampIncreases :: [Transaction] -> [Transaction] -> Bool
propTimestampIncreases txsA txsB =
    let (lrsCheckpoint, txs) = massageData txsA txsB
        (lrsCheckpoint', _) = determineNewTransactions lrsCheckpoint txs
    in lrsTimestamp lrsCheckpoint <= lrsTimestamp lrsCheckpoint'

propNewTransactions txsA txsB =
    let (lrsCheckpoint, txs) = massageData txsA txsB
        knownTxIDs = lrsKnownTxIDs lrsCheckpoint
        (_, newTxs) = determineNewTransactions lrsCheckpoint txs
    in all (\tx -> tTxid tx `notElem` knownTxIDs) newTxs
