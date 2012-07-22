{-# LANGUAGE OverloadedStrings #-}
module Network.BitcoinRPC.MarkerAddressesTest
    ( markerAdressesTests
    ) where

import Control.Arrow
import Data.Foldable
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Test)

import Network.BitcoinRPC.Events
import Network.BitcoinRPC.MarkerAddresses
import Network.BitcoinRPC.TestTypes
import Network.BitcoinRPC.Types

propSumsMatch arbListA arbListB =
    let listA = map ((***) unABAddr unABAmount) arbListA
        listB = map ((***) unABAddr unABAmount) arbListB
        sumA = sumAcceptedMarkerAmounts listA
        sumB = sumAcceptedMarkerAmounts listB
        sumTotal = sumAcceptedMarkerAmounts (listA ++ listB)
        sumTotal' = sumAcceptedMarkerAmounts (sumA ++ sumB)
    in sumTotal == sumTotal'

propSizeLessOrEqual arbListA =
    let listA = map ((***) unABAddr unABAmount) arbListA
        sumA = sumAcceptedMarkerAmounts listA
        sumA' = sumAcceptedMarkerAmounts sumA
    in length sumA <= length listA
        && length sumA == length sumA'

makeTransactionEvents :: TransactionID-> BitcoinAddress-> BitcoinAmount-> [BitcoinAddress]-> (BitcoinEvent, BitcoinEvent, BitcoinEvent, BitcoinEvent)
makeTransactionEvents txid address amount origins =
    let tx = ReceiveTx 0 amount address 0 txid 0
        utxid = UniqueTransactionID txid 0
        eventNew = NewTransaction utxid tx origins
        eventUpdate = TransactionUpdate utxid 1
        eventAcc = TransactionAccepted utxid
        eventDis = TransactionDisappeared utxid
    in (eventNew, eventUpdate, eventAcc, eventDis)

checkTestData :: (MAStore, [(String, [BitcoinEvent], Int)]) -> IO ()
checkTestData (initialStore, entries) = do
    foldlM check initialStore entries
    return ()
  where
    check store (msg, events, expectedReply) = do
        let (store', fEvents) = processEvents store events
            errMsg = "Unexpected result in step \"" ++ msg ++ "\"."
                        ++ " Expected " ++ show expectedReply ++ " event(s),"
                        ++ " but got: " ++ show fEvents
                        ++ " also: " ++ show store'
        assertBool errMsg (length fEvents == expectedReply)
        return store'

standardTransactionTestData :: (MAStore, [(String, [BitcoinEvent], Int)])
standardTransactionTestData =
    let store = initMarkerAddressStore []
        (new, update, acc, _) = makeTransactionEvents
                                    (TransactionID "abc") (BitcoinAddress "1ab")
                                    1 []
    in (store, [ ("transaction appears", [new], 0)
               , ("transaction is updated", [update], 0)
               , ("transaction is accepted", [acc], 1)
               ] )

test1 = testCase "standard transaction" $
            checkTestData standardTransactionTestData

markerTransactionTestData =
    let store = initMarkerAddressStore [(BitcoinAddress "1def", 1)]
        (new, update, acc, _) = makeTransactionEvents
                                    (TransactionID "abc") (BitcoinAddress "1ab")
                                    1 [BitcoinAddress "1def"]
    in (store, [ ("transaction appears", [new], 1)
               , ("transaction is updated", [update], 0)
               , ("transaction is accepted", [acc], 0)
               ] )

test2 = testCase "marker transaction" $
            checkTestData markerTransactionTestData

standardTransactionDisappearingTestData :: (MAStore, [(String, [BitcoinEvent], Int)])
standardTransactionDisappearingTestData =
    let store = initMarkerAddressStore []
        (new, update, _, dis) = makeTransactionEvents
                                    (TransactionID "abc") (BitcoinAddress "1ab")
                                    1 []
    in (store, [ ("transaction appears", [new], 0)
               , ("transaction is updated", [update], 0)
               , ("transaction disappears", [dis], 0)
               ] )

test3 = testCase "standard transaction, disappearing" $
            checkTestData standardTransactionDisappearingTestData

markerTransactionDisappearingTestData =
    let store = initMarkerAddressStore [(BitcoinAddress "1def", 1)]
        (new1, update1, _, dis1) = makeTransactionEvents
                                    (TransactionID "abc") (BitcoinAddress "1ab")
                                    1 [BitcoinAddress "1def"]
        (new2, update2, acc2, _) = makeTransactionEvents
                                    (TransactionID "def") (BitcoinAddress "1de")
                                    1 [BitcoinAddress "1def"]
    in (store, [ ("transaction appears", [new1], 1)
               , ("transaction is updated", [update1], 0)
               , ("transaction is accepted", [dis1], 1)
               , ("second transaction appears", [new2], 0)
                        -- is no longer accepted right away
               , ("second transaction is updated", [update2], 0)
               , ("second transaction is accepted", [acc2], 1)
               ] )

test4 = testCase "marker transaction, disappearing" $
            checkTestData markerTransactionDisappearingTestData

complexScenarioTestData =
    let store = initMarkerAddressStore [(BitcoinAddress "marker", 1)]
        (new1, update1, acc1, _) = makeTransactionEvents
                                    (TransactionID "t1") (BitcoinAddress "a1")
                                    1 [BitcoinAddress "marker"]
        (new2, update2, acc2, _) = makeTransactionEvents
                                    (TransactionID "t2") (BitcoinAddress "a2")
                                    1 [BitcoinAddress "marker"]
        (new3, update3, acc3, _) = makeTransactionEvents
                                    (TransactionID "t3") (BitcoinAddress "a3")
                                    1 []
    in (store, [ ("t1 & t3 new", [new1, new3], 1)
               , ("t2 new", [new2], 0)  -- pending amount is too high
               , ("t3 accepted", [update3, acc3], 1)
               , ("t1 & t2 updated", [update1, update2], 0)
               , ("t1 accepted", [acc1], 1) -- t2 below limit now
               , ("t2 accepted", [acc2], 0)
               ] )

test5 = testCase "complex scenario" $
            checkTestData complexScenarioTestData

markerAdressesTests :: [Test]
markerAdressesTests = [ testProperty "sums match" propSumsMatch
                      , testProperty "sum size is less or equal" propSizeLessOrEqual
                      , test1, test2, test3, test4, test5
                      ]

main :: IO ()
main = defaultMain [test1, test2, test3, test4, test5]
