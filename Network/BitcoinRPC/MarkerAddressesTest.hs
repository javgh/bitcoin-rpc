module Network.BitcoinRPC.MarkerAddressesTest
    ( markerAdressesTests
    ) where

import Control.Arrow
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Network.BitcoinRPC.MarkerAddresses
import Network.BitcoinRPC.TestTypes

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

markerAdressesTests = [ testProperty "sums match" propSumsMatch
                      , testProperty "sum size is less or equal" propSizeLessOrEqual
                      ]
