module Network.BitcoinRPC.TestTypes
    ( ArbBitcoinAddress(..)
    , ArbTransactionID(..)
    , ArbBitcoinAmount(..)
    , ArbTransaction(..)
    ) where

import Control.Applicative
import Test.QuickCheck

import qualified Data.Text as T

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
        a <- choose ('a', 'z')
        b <- choose ('a', 'z')
        c <- choose ('a', 'z')
        return $ ABAddr (BitcoinAddress (T.pack ['1', a, b, c]))

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
