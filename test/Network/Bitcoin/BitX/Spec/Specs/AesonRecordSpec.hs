{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Network.Bitcoin.BitX.Spec.Specs.AesonRecordSpec
    (
    spec
    ) where

import Test.Hspec
import Network.Bitcoin.BitX.Types
import Data.Time.Clock.POSIX
import Network.Bitcoin.BitX.Spec.Common

spec :: Spec
spec =
  describe "FromJSON to Record" $ do
    it "BitXError is parsed properly" $
      recordAesCheck
        "{\"error\" : \"oops\", \"error_code\" : \"ABadError\"}"
        BitXError {bitXErrorError = "oops", bitXErrorErrorCode = "ABadError"}
    it "Ticker is parsed properly" $
      recordAesCheck
        "{\"timestamp\":1431811395699,\"bid\":\"3083.00\",\"ask\":\"3115.00\",\
            \ \"last_trade\":\"3116.00\",\"rolling_24_hour_volume\":\"19.776608\",\"pair\":\"XBTZAR\"}"
        Ticker {
             tickerTimestamp = posixSecondsToUTCTime 1431811395.699,
             tickerBid = Just 3083,
             tickerAsk = Just 3115,
             tickerLastTrade = Just 3116,
             tickerRolling24HourVolume = 19.776608,
             tickerPair = XBTZAR}
    it "Ticker with missing fields is parsed properly" $
      recordAesCheck
        "{\"timestamp\":1431811395699,\"bid\":\"3083.00\",\
            \ \"last_trade\":\"3116.00\",\"rolling_24_hour_volume\":\"19.776608\",\"pair\":\"XBTZAR\"}"
        Ticker {
             tickerTimestamp = posixSecondsToUTCTime 1431811395.699,
             tickerBid = Just 3083,
             tickerAsk = Nothing,
             tickerLastTrade = Just 3116,
             tickerRolling24HourVolume = 19.776608,
             tickerPair = XBTZAR}
    it "Balance is parsed properly" $
      recordAesCheck
        "{\"account_id\":\"314159\",\"asset\":\"ZAR\",\"balance\":\"2159.15\",\"reserved\":\"320.43\",\
            \ \"unconfirmed\":\"175.34\"}"
        Balance
            {balanceId = "314159",
             balanceAsset = ZAR,
             balanceBalance = 2159.15,
             balanceReserved = 320.43,
             balanceUnconfirmed = 175.34}
    it "Order is parsed properly" $
      recordAesCheck
        "{\"volume\":\"314159.26\",\"price\":4321.00}"
        Order {orderVolume = 314159.26, orderPrice = 4321}
    it "WithdrawalRequest is parsed properly" $
      recordAesCheck
        "{\"status\":\"PENDING\",\"id\":\"271828\"}"
        WithdrawalRequest
            {withdrawalRequestStatus = PENDING,
            withdrawalRequestId = "271828" }
    it "Tickers is parsed properly" $
      recordAesCheck
        "{\"tickers\":[{\"timestamp\":1431811395699,\"bid\":\"3083.00\",\"ask\":\"3115.00\",\
            \ \"last_trade\":\"3116.00\",\"rolling_24_hour_volume\":\"19.776608\",\
            \ \"pair\":\"XBTZAR\"}]}"
        [tickerInner]
    it "Orderbook is parsed properly" $
      recordAesCheck
        "{\"timestamp\":1431811395699,\"bids\":[{\"volume\":\"654.98\",\"price\":\"3789.00\"}],\
            \ \"asks\":[{\"volume\":\"654.98\",\"price\":\"3789.00\"}]}"
        Orderbook
            {orderbookTimestamp = posixSecondsToUTCTime 1431811395.699,
             orderbookBids = [orderInner],
             orderbookAsks = [orderInner]}
    it "Trade is parsed properly" $
      recordAesCheck
        "{\"timestamp\":1431811395699,\"volume\":\"6754.09\",\"price\":\"5327.00\", \"is_buy\": true}"
        Trade
            {tradeTimestamp = posixSecondsToUTCTime 1431811395.699,
             tradeVolume = 6754.09,
             tradePrice = 5327,
             tradeIsBuy = True}
    it "PrivateOrder is parsed properly" $
      recordAesCheck
        "{\"base\":\"568.7\", \"counter\":3764.2,\"creation_timestamp\":478873467, \
            \ \"expiration_timestamp\":8768834222, \"completed_timestamp\":6511257825, \"fee_base\":\"3687.3\", \"fee_counter\":12.9,\
            \ \"limit_price\":765.00,\"limit_volume\":55.2,\"order_id\":\"83YG\",\"pair\":\"XBTMYR\",\
            \ \"state\":\"COMPLETE\",\"type\":\"BID\"}"
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
             privateOrderOrderType = BID}
    it "PrivateOrders is parsed properly" $
      recordAesCheck
        "{\"orders\":[{\"base\":\"568.7\", \"counter\":3764.2,\"creation_timestamp\":478873467, \
            \ \"expiration_timestamp\":8768834222, \"completed_timestamp\":6511257825, \"fee_base\":\"3687.3\", \"fee_counter\":12.9,\
            \ \"limit_price\":765.00,\"limit_volume\":55.2,\"order_id\":\"83YG\",\"pair\":\"XBTMYR\",\
            \ \"state\":\"COMPLETE\",\"type\":\"BID\"}]}"
        [privateOrderInner]
    it "OrderID is parsed properly" $
      recordAesCheck
        "{\"order_id\":\"57983\"}"
        ("57983" :: OrderID)
    it "PublicTrades is parsed properly" $
      recordAesCheck
        "{\"trades\":[{\"timestamp\":1431811395699,\"volume\":\"6754.09\",\"price\":\"5327.00\",\"is_buy\":false}], \
            \ \"currency\":\"ZAR\"}"
         [tradeInner]
    it "RequestSuccess is parsed properly" $
      recordAesCheck
        "{\"success\":true}"
        (True :: RequestSuccess)
    it "WithdrawalRequest is parsed properly" $
      recordAesCheck
        "{\"status\":\"PENDING\", \"id\":\"7yrfU4987\"}"
        WithdrawalRequest
            {withdrawalRequestStatus = PENDING,
             withdrawalRequestId = "7yrfU4987" }
    it "FundingAddress is parsed properly" $
      recordAesCheck
        "{\"asset\":\"ZAR\", \"address\":\"093gu959t894G\", \"total_received\":\"432.5\", \
            \ \"total_unconfirmed\":\"0.023\"}"
        FundingAddress
            {fundingAddressAsset = ZAR,
             fundingAddressAddress = "093gu959t894G",
             fundingAddressTotalReceived = 432.5,
             fundingAddressTotalUnconfirmed = 0.023}
    it "Transaction is parsed properly" $
      recordAesCheck
        "{\"row_index\":1,\"timestamp\":1387527013000,\"balance\":0.0199, \"available\":0.0299, \
            \ \"account_id\":\"3485527347968330182\", \"balance_delta\":0.0399, \
            \ \"available_delta\":0.0099,  \"currency\":\"XBT\",\"description\":\"Bought BTC 0.01 for R 79.00\"}"
        Transaction
            {transactionRowIndex = 1,
             transactionTimestamp = posixSecondsToUTCTime 1387527013,
             transactionBalance = 0.0199,
             transactionAvailable = 0.0299,
             transactionBalanceDelta = 0.0399,
             transactionAvailableDelta = 0.0099,
             transactionCurrency = XBT,
             transactionDescription = "Bought BTC 0.01 for R 79.00"}
    it "Transactions is parsed properly" $
      recordAesCheck
        "{\"transactions\":[{\"row_index\":1,\"timestamp\":1387527013000,\"balance\":0.0199, \"available\":0.0299, \
            \ \"account_id\":\"3485527347968330182\", \"balance_delta\":0.0399, \
            \ \"available_delta\":0.0099,  \"currency\":\"XBT\",\"description\":\"Bought BTC 0.01 for R 79.00\"}]}"
        [transactionInner]
    it "PrivateTrade is parsed properly" $
      recordAesCheck
        "{\"base\": \"0.147741\", \"counter\": \"1549.950831\", \"fee_base\": \"0.90\", \"fee_counter\": \"0.00\", \
            \ \"is_buy\": false, \"order_id\": \"BXMC2CJ7HNB88U4\", \"pair\": \"XBTZAR\", \"price\": \"10491.00\", \
            \ \"timestamp\": 1467138492909, \"type\": \"BID\", \"volume\": \"0.147741\" }"
        PrivateTrade
            {privateTradeBase = 0.147741,
             privateTradeCounter = 1549.950831,
             privateTradeFeeBase = 0.9,
             privateTradeFeeCounter = 0,
             privateTradeIsBuy = False,
             privateTradeOrderId = "BXMC2CJ7HNB88U4",
             privateTradePair = XBTZAR,
             privateTradePrice = 10491,
             privateTradeTimestamp = posixSecondsToUTCTime 1467138492.909,
             privateTradeOrderType = BID,
             privateTradeVolume = 0.147741}
    describe "PrivateTrades" $ do
      it "parses a canned example correctly" $
        recordAesCheck
          "{\"trades\":[{\"base\": \"0.147741\", \"counter\": \"1549.950831\", \"fee_base\": \"0.90\", \"fee_counter\": \"0.00\", \
              \ \"is_buy\": false, \"order_id\": \"BXMC2CJ7HNB88U4\", \"pair\": \"XBTZAR\", \"price\": \"10491.00\", \
              \ \"timestamp\": 1467138492909, \"type\": \"BID\", \"volume\": \"0.147741\" }]}"
           [privateTradeInner]
      it "parses null as an empty list" $
        recordAesCheck
          "{\"trades\":null}"
          ([] :: [PrivateTrade])
    it "FeeInfo is parsed properly" $
      recordAesCheck
        "{\"maker_fee\": \"0.00\", \"taker_fee\": \"0.10\", \"thirty_day_volume\": \"0.894342\"}"
        FeeInfo
            {feeInfoMakerFee = 0,
             feeInfoTakerFee = 0.1,
             feeInfoThirtyDayVolume = 0.894342}

tickerInner :: Ticker
tickerInner =
    Ticker (posixSecondsToUTCTime 1431811395.699)
        (Just 3083)
        (Just 3115)
        (Just 3116)
        19.776608
        XBTZAR

orderInner :: Order
orderInner =
    Order 654.98 3789

tradeInner :: Trade
tradeInner =
    Trade {
         tradeTimestamp = posixSecondsToUTCTime 1431811395.699,
         tradeVolume = 6754.09,
         tradePrice = 5327,
         tradeIsBuy = False}

privateOrderInner :: PrivateOrder
privateOrderInner =
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
        privateOrderOrderType = BID}

transactionInner :: Transaction
transactionInner =
    Transaction
        {transactionRowIndex = 1,
         transactionTimestamp = posixSecondsToUTCTime 1387527013,
         transactionBalance = 0.0199,
         transactionAvailable = 0.0299,
         transactionBalanceDelta = 0.0399,
         transactionAvailableDelta = 0.0099,
         transactionCurrency = XBT,
         transactionDescription = "Bought BTC 0.01 for R 79.00"}

privateTradeInner :: PrivateTrade
privateTradeInner =
    PrivateTrade
        {privateTradeBase = 0.147741,
          privateTradeCounter = 1549.950831,
          privateTradeFeeBase = 0.9,
          privateTradeFeeCounter = 0,
          privateTradeIsBuy = False,
          privateTradeOrderId = "BXMC2CJ7HNB88U4",
          privateTradePair = XBTZAR,
          privateTradePrice = 10491,
          privateTradeTimestamp = posixSecondsToUTCTime 1467138492.909,
          privateTradeOrderType = BID,
          privateTradeVolume = 0.147741}
