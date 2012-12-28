{-# LANGUAGE DeriveGeneric #-}

-- | Provides some glue code to use the module 'Network.BitcoinRPC.MarkerAddresses'
-- in combination with 'Network.BitcoinRPC.Events' and automatically filter
-- everything based on the marker address settings.

module Network.BitcoinRPC.Events.MarkerAddresses
    ( initialFilteredEventTaskState
    , initFilteredBitcoinEventTask
    , waitForFilteredBitcoinEvents
    , killFilteredBitcoinEventTask
    , updateMarkerAddresses
    , listPendingTransactions
    , listMarkerAdressStatus
    , FilteredBitcoinEventTaskHandle
    , FilteredEventTaskState
    , MA.PendingReason(..)
    , MA.FilteredBitcoinEvent(..)
    ) where

import Control.Concurrent
import Control.Watchdog
import Data.Serialize
import GHC.Generics

import Network.BitcoinRPC
import Network.BitcoinRPC.Events

import qualified Network.BitcoinRPC.MarkerAddresses as MA

data FilteredBitcoinEventTaskHandle = FilteredBitcoinEventTaskHandle
                                            { fbetBetHandle :: BitcoinEventTaskHandle
                                            , fbetWrappedMAStore :: MVar MA.MAStore
                                            }

data FilteredEventTaskState = FilteredEventTaskState
                                    { fetsMAStore :: MA.MAStore
                                    , fetsEventTaskState :: EventTaskState
                                    }
                              deriving (Show, Generic)

instance Serialize FilteredEventTaskState

-- | Returns an initial 'FilteredEventTaskState' with no marker addresses
-- configured. Use 'updateMarkerAddresses' to add marker addresses.
initialFilteredEventTaskState :: FilteredEventTaskState
initialFilteredEventTaskState =
    FilteredEventTaskState { fetsMAStore = MA.initMarkerAddressStore []
                           , fetsEventTaskState = initialEventTaskState
                           }

updateMarkerAddresses :: FilteredEventTaskState-> [(BitcoinAddress, BitcoinAmount)] -> FilteredEventTaskState
updateMarkerAddresses state markerAddresses =
    let maStore' = MA.updateMarkerAddresses (fetsMAStore state) markerAddresses
    in state { fetsMAStore = maStore' }

listPendingTransactions :: FilteredEventTaskState -> [(Transaction, MA.PendingReason)]
listPendingTransactions state = MA.listPendingTransactions (fetsMAStore state)

listMarkerAdressStatus :: FilteredEventTaskState-> [(BitcoinAddress, Bool, BitcoinAmount, BitcoinAmount)]
listMarkerAdressStatus state = MA.listMarkerAdressStatus (fetsMAStore state)

initFilteredBitcoinEventTask :: Maybe WatchdogLogger
                             -> RPCAuth
                             -> FilePath    -- ^  PID will be written here to get
                                            -- notifications from Bitcoin daemon
                             -> (TransactionHeader -> Bool)
                                            -- ^ test to decide whether a
                                            -- transaction can be accepted
                             -> FilteredEventTaskState  -- ^ state from
                                                        -- which to resume
                             -> IO (FilteredBitcoinEventTaskHandle)
initFilteredBitcoinEventTask mLogger auth pidfile acceptTest state = do
    betHandle <- initBitcoinEventTask mLogger auth pidfile
                                        acceptTest (fetsEventTaskState state)
    wrappedMAStore <- newMVar $ fetsMAStore state
    return $ FilteredBitcoinEventTaskHandle betHandle wrappedMAStore

-- | Wait for new Bitcoin events, but already filter them based on the
-- marker address configuration. This might sometimes return an empty list of
-- events.
waitForFilteredBitcoinEvents :: FilteredBitcoinEventTaskHandle-> IO (FilteredEventTaskState, [MA.FilteredBitcoinEvent])
waitForFilteredBitcoinEvents fbetHandle = do
    let betHandle = fbetBetHandle fbetHandle
    maStore <- readMVar $ fbetWrappedMAStore fbetHandle
    (etState', events) <- waitForBitcoinEvents betHandle
    let (maStore', fEvents) = MA.processEvents maStore events
    _ <- swapMVar (fbetWrappedMAStore fbetHandle) maStore'
    let fbetState = FilteredEventTaskState
                           { fetsMAStore = maStore'
                           , fetsEventTaskState = etState'
                           }
    return (fbetState, fEvents)

killFilteredBitcoinEventTask :: FilteredBitcoinEventTaskHandle -> IO ()
killFilteredBitcoinEventTask fbetHandle =
    killBitcoinEventTask (fbetBetHandle fbetHandle)
