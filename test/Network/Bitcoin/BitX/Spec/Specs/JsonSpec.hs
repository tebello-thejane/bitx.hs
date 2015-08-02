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
    it "QuotedDecimal is parsed whether quoted or not, for floating point numbers" $
      recordAesCheck
        "{\"volume\":314159.23,\"price\":\"4321.56\"}"
        (Order 314159.23 4321.56)
    it "QuotedDecimal is parsed whether quoted or not, for integral numbers" $
      recordAesCheck
        "{\"volume\":314159,\"price\":\"4321\"}"
        (Order 314159 4321)
    it "OrderType BUY is parsed properly" $
      recordAesCheck
        "{\"base\":\"568.7\", \"counter\":3764.2,\"creation_timestamp\":478873467, \
            \\"expiration_timestamp\":8768834222, \"fee_base\":\"3687.3\", \"fee_counter\":12.9,\
            \\"limit_price\":765,\"limit_volume\":55.2,\"order_id\":\"83YG\",\"pair\":\"NADXBT\",\
            \\"state\":\"COMPLETE\",\"type\":\"BUY\"}"
        PrivateOrder
            {privateOrderBase = 568.7,
             privateOrderCounter = 3764.2,
             privateOrderCreationTimestamp = posixSecondsToUTCTime 478873.467,
             privateOrderExpirationTimestamp = posixSecondsToUTCTime 8768834.222,
             privateOrderFeeBase = 3687.3,
             privateOrderFeeCounter = 12.9,
             privateOrderLimitPrice = 765,
             privateOrderLimitVolume = 55.2,
             privateOrderId = "83YG",
             privateOrderPair = NADXBT,
             privateOrderState = COMPLETE,
             privateOrderOrderType = BID }
    it "OrderType BID is parsed properly" $
      recordAesCheck
        "{\"base\":\"568.7\", \"counter\":3764.2,\"creation_timestamp\":478873467, \
            \\"expiration_timestamp\":8768834222, \"fee_base\":\"3687.3\", \"fee_counter\":12.9,\
            \\"limit_price\":765,\"limit_volume\":55.2,\"order_id\":\"83YG\",\"pair\":\"NADXBT\",\
            \\"state\":\"COMPLETE\",\"type\":\"BID\"}"
        PrivateOrder
            {privateOrderBase = 568.7,
             privateOrderCounter = 3764.2,
             privateOrderCreationTimestamp = posixSecondsToUTCTime 478873.467,
             privateOrderExpirationTimestamp = posixSecondsToUTCTime 8768834.222,
             privateOrderFeeBase = 3687.3,
             privateOrderFeeCounter = 12.9,
             privateOrderLimitPrice = 765,
             privateOrderLimitVolume = 55.2,
             privateOrderId = "83YG",
             privateOrderPair = NADXBT,
             privateOrderState = COMPLETE,
             privateOrderOrderType = BID }
    it "OrderType ASK is parsed properly" $
      recordAesCheck
        "{\"base\":\"568.7\", \"counter\":3764.2,\"creation_timestamp\":478873467, \
            \\"expiration_timestamp\":8768834222, \"fee_base\":\"3687.3\", \"fee_counter\":12.9,\
            \\"limit_price\":765,\"limit_volume\":55.2,\"order_id\":\"83YG\",\"pair\":\"NADXBT\",\
            \\"state\":\"COMPLETE\",\"type\":\"ASK\"}"
        PrivateOrder
            {privateOrderBase = 568.7,
             privateOrderCounter = 3764.2,
             privateOrderCreationTimestamp = posixSecondsToUTCTime 478873.467,
             privateOrderExpirationTimestamp = posixSecondsToUTCTime 8768834.222,
             privateOrderFeeBase = 3687.3,
             privateOrderFeeCounter = 12.9,
             privateOrderLimitPrice = 765,
             privateOrderLimitVolume = 55.2,
             privateOrderId = "83YG",
             privateOrderPair = NADXBT,
             privateOrderState = COMPLETE,
             privateOrderOrderType = ASK }
    it "OrderType SELL is parsed properly" $
      recordAesCheck
        "{\"base\":\"568.7\", \"counter\":3764.2,\"creation_timestamp\":478873467, \
            \\"expiration_timestamp\":8768834222, \"fee_base\":\"3687.3\", \"fee_counter\":12.9,\
            \\"limit_price\":765,\"limit_volume\":55.2,\"order_id\":\"83YG\",\"pair\":\"NADXBT\",\
            \\"state\":\"COMPLETE\",\"type\":\"SELL\"}"
        PrivateOrder
            {privateOrderBase = 568.7,
             privateOrderCounter = 3764.2,
             privateOrderCreationTimestamp = posixSecondsToUTCTime 478873.467,
             privateOrderExpirationTimestamp = posixSecondsToUTCTime 8768834.222,
             privateOrderFeeBase = 3687.3,
             privateOrderFeeCounter = 12.9,
             privateOrderLimitPrice = 765,
             privateOrderLimitVolume = 55.2,
             privateOrderId = "83YG",
             privateOrderPair = NADXBT,
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
