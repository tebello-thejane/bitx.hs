{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Network.Bitcoin.BitX.Spec.Specs.JsonSpec
    (
    spec
    ) where

import Test.Hspec
import Network.Bitcoin.BitX.Types
import Network.Bitcoin.BitX.Spec.Common
import Data.Time.Clock.POSIX

spec :: Spec
spec =
  describe "FromJSON intances" $ do
    it "QuotedScientific is parsed when quoted, for floating point numbers" $
      recordAesCheck
        "{\"volume\":\"314159.23\",\"price\":\"4321\"}"
        (Order 314159.23 4321)
    it "QuotedScientific is parsed when not quoted, for floating point numbers" $
      recordAesCheck
        "{\"volume\":314159.23,\"price\":\"4321\"}"
        (Order 314159.23 4321)
    it "QuotedScientific is parsed when quoted, for integral numbers" $
      recordAesCheck
        "{\"volume\":\"314159\",\"price\":\"4321\"}"
        (Order 314159 4321)
    it "QuotedScientific is parsed when not quoted, for integral numbers" $
      recordAesCheck
        "{\"volume\":314159,\"price\":\"4321\"}"
        (Order 314159 4321)
    it "QuotedInt is parsed when quoted" $
      recordAesCheck
        "{\"volume\":0,\"price\":\"4321.00\"}"
        (Order 0 4321)
    it "QuotedInt is parsed when not quoted" $
      recordAesCheck
        "{\"volume\":0,\"price\":4321.00}"
        (Order 0 4321)
    it "OrderType BUY is parsed properly" $
      recordAesCheck
        "{\"base\":\"568.7\", \"counter\":3764.2,\"creation_timestamp\":478873467, \
            \\"expiration_timestamp\":8768834222, \"completed_timestamp\":6511257825, \"fee_base\":\"3687.3\", \"fee_counter\":12.9,\
            \\"limit_price\":765.00,\"limit_volume\":55.2,\"order_id\":\"83YG\",\"pair\":\"XBTMYR\",\
            \\"state\":\"COMPLETE\",\"type\":\"BUY\"}"
        PrivateOrder
            {privateOrderBase = 568.7,
             privateOrderCounter = 3764.2,
             privateOrderCreationTimestamp = posixSecondsToUTCTime 478873.467,
             privateOrderExpirationTimestamp = posixSecondsToUTCTime 8768834.222,
             privateOrderCompletedTimestamp = posixSecondsToUTCTime 6511257.825,
             privateOrderFeeBase = 3687.3,
             privateOrderFeeCounter = 12.9,
             privateOrderLimitPrice = 765,
             privateOrderLimitVolume = 55.2,
             privateOrderId = "83YG",
             privateOrderPair = XBTMYR,
             privateOrderState = COMPLETE,
             privateOrderOrderType = BID }
    it "OrderType BID is parsed properly" $
      recordAesCheck
        "{\"base\":\"568.7\", \"counter\":3764.2,\"creation_timestamp\":478873467, \
            \\"expiration_timestamp\":8768834222, \"completed_timestamp\":6511257825, \"fee_base\":\"3687.3\", \"fee_counter\":12.9,\
            \\"limit_price\":765.00,\"limit_volume\":55.2,\"order_id\":\"83YG\",\"pair\":\"XBTMYR\",\
            \\"state\":\"COMPLETE\",\"type\":\"BID\"}"
        PrivateOrder
            {privateOrderBase = 568.7,
             privateOrderCounter = 3764.2,
             privateOrderCreationTimestamp = posixSecondsToUTCTime 478873.467,
             privateOrderExpirationTimestamp = posixSecondsToUTCTime 8768834.222,
             privateOrderCompletedTimestamp = posixSecondsToUTCTime 6511257.825,
             privateOrderFeeBase = 3687.3,
             privateOrderFeeCounter = 12.9,
             privateOrderLimitPrice = 765,
             privateOrderLimitVolume = 55.2,
             privateOrderId = "83YG",
             privateOrderPair = XBTMYR,
             privateOrderState = COMPLETE,
             privateOrderOrderType = BID }
    it "OrderType ASK is parsed properly" $
      recordAesCheck
        "{\"base\":\"568.7\", \"counter\":3764.2,\"creation_timestamp\":478873467, \
            \\"expiration_timestamp\":8768834222, \"completed_timestamp\":6511257825, \"fee_base\":\"3687.3\", \"fee_counter\":12.9,\
            \\"limit_price\":765.00,\"limit_volume\":55.2,\"order_id\":\"83YG\",\"pair\":\"XBTMYR\",\
            \\"state\":\"COMPLETE\",\"type\":\"ASK\"}"
        PrivateOrder
            {privateOrderBase = 568.7,
             privateOrderCounter = 3764.2,
             privateOrderCreationTimestamp = posixSecondsToUTCTime 478873.467,
             privateOrderExpirationTimestamp = posixSecondsToUTCTime 8768834.222,
             privateOrderCompletedTimestamp = posixSecondsToUTCTime 6511257.825,
             privateOrderFeeBase = 3687.3,
             privateOrderFeeCounter = 12.9,
             privateOrderLimitPrice = 765,
             privateOrderLimitVolume = 55.2,
             privateOrderId = "83YG",
             privateOrderPair = XBTMYR,
             privateOrderState = COMPLETE,
             privateOrderOrderType = ASK }
    it "OrderType SELL is parsed properly" $
      recordAesCheck
        "{\"base\":\"568.7\", \"counter\":3764.2,\"creation_timestamp\":478873467, \
            \\"expiration_timestamp\":8768834222, \"completed_timestamp\":6511257825, \"fee_base\":\"3687.3\", \"fee_counter\":12.9,\
            \\"limit_price\":765.00,\"limit_volume\":55.2,\"order_id\":\"83YG\",\"pair\":\"XBTMYR\",\
            \\"state\":\"COMPLETE\",\"type\":\"SELL\"}"
        PrivateOrder
            {privateOrderBase = 568.7,
             privateOrderCounter = 3764.2,
             privateOrderCreationTimestamp = posixSecondsToUTCTime 478873.467,
             privateOrderExpirationTimestamp = posixSecondsToUTCTime 8768834.222,
             privateOrderCompletedTimestamp = posixSecondsToUTCTime 6511257.825,
             privateOrderFeeBase = 3687.3,
             privateOrderFeeCounter = 12.9,
             privateOrderLimitPrice = 765,
             privateOrderLimitVolume = 55.2,
             privateOrderId = "83YG",
             privateOrderPair = XBTMYR,
             privateOrderState = COMPLETE,
             privateOrderOrderType = ASK }
    it "RequestStatus COMPLETE is parsed properly" $
      recordAesCheck
        "{\"status\":\"COMPLETE\", \"id\":\"\"}"
        WithdrawalRequest
            {withdrawalRequestStatus = COMPLETE,
            withdrawalRequestId = "" }
    it "RequestStatus COMPLETED is parsed properly" $
      recordAesCheck
        "{\"status\":\"COMPLETED\", \"id\":\"\"}"
        WithdrawalRequest
            {withdrawalRequestStatus = COMPLETE,
            withdrawalRequestId = "" }
