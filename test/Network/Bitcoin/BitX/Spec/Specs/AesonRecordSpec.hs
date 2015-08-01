{-# LANGUAGE OverloadedStrings, QuasiQuotes, DataKinds #-}

module Network.Bitcoin.BitX.Spec.Specs.AesonRecordSpec
    (
    spec
    ) where

import Test.Hspec
import Network.Bitcoin.BitX.Types
import Data.Time.Clock.POSIX
import Network.Bitcoin.BitX.Spec.Common

spec :: Spec
spec = do
  describe "FromJSON to Record" $ do
    it "BitXError is parsed properly" $ do
      recordAesCheck
        "{\"error\" : \"oops\", \"error_code\" : \"ABadError\"}"
        (BitXError {bitXErrorError = "oops", bitXErrorErrorCode = "ABadError"})
    it "Ticker is parsed properly" $ do
      recordAesCheck
        "{\"timestamp\":1431811395699,\"bid\":\"3083.00\",\"ask\":\"3115.00\",\
            \ \"last_trade\":\"3116.00\",\"rolling_24_hour_volume\":\"19.776608\",\"pair\":\"XBTZAR\"}"
        (Ticker {
             tickerTimestamp = (posixSecondsToUTCTime 1431811395.699),
             tickerBid = 3083.0,
             tickerAsk = 3115.00,
             tickerLastTrade = 3116.00,
             tickerRolling24HourVolume = 19.776608,
             tickerPair = XBTZAR})
    it "Balance is parsed properly" $ do
      recordAesCheck
        "{\"account_id\":\"314159\",\"asset\":\"ZAR\",\"balance\":\"2159.15\",\"reserved\":\"320\",\
            \ \"unconfirmed\":\"175\"}"
        (Balance
            {balanceId = "314159",
             balanceAsset = ZAR,
             balanceBalance = 2159.15,
             balanceReserved = 320,
             balanceUnconfirmed = 175} )
    it "Order is parsed properly" $ do
      recordAesCheck
        "{\"volume\":\"314159\",\"price\":\"4321\"}"
        (Order {orderVolume = 314159, orderPrice = 4321})
    it "WithdrawalRequest is parsed properly" $ do
      recordAesCheck
        "{\"status\":\"PENDING\",\"id\":\"271828\"}"
        (WithdrawalRequest
            {withdrawalRequestStatus = PENDING,
            withdrawalRequestId = "271828" } )
    it "Tickers is parsed properly" $ do
      recordAesCheck
        "{\"tickers\":[{\"timestamp\":1431811395699,\"bid\":\"3083.00\",\"ask\":\"3115.00\",\
            \ \"last_trade\":\"3116.00\",\"rolling_24_hour_volume\":\"19.776608\",\
            \ \"pair\":\"XBTZAR\"}]}"
        [tickerInner]
    it "Orderbook is parsed properly" $ do
      recordAesCheck
        "{\"timestamp\":1431811395699,\"bids\":[{\"volume\":\"654.98\",\"price\":\"3789.66\"}],\
            \ \"asks\":[{\"volume\":\"654.98\",\"price\":\"3789.66\"}]}"
        (Orderbook
            {orderbookTimestamp = (posixSecondsToUTCTime 1431811395.699),
             orderbookBids = [orderInner],
             orderbookAsks = [orderInner]})
    it "Trade is parsed properly" $ do
      recordAesCheck
        "{\"timestamp\":1431811395699,\"volume\":\"6754.09\",\"price\":\"5327.765\"}"
        (Trade
            {tradeTimestamp = (posixSecondsToUTCTime 1431811395.699),
             tradeVolume = 6754.09,
             tradePrice = 5327.765 } )
    it "PublicTrades is parsed properly" $ do
      recordAesCheck
        "{\"trades\":[{\"timestamp\":1431811395699,\"volume\":\"6754.09\",\
            \ \"price\":\"5327.765\"}],\"currency\":\"ZAR\"}"
        [tradeInner]
    it "PrivateOrder is parsed properly" $ do
      recordAesCheck
        "{\"base\":\"568.7\", \"counter\":3764.2,\"creation_timestamp\":478873467, \
            \ \"expiration_timestamp\":8768834222, \"fee_base\":\"3687.3\", \"fee_counter\":12.9,\
            \ \"limit_price\":765,\"limit_volume\":55.2,\"order_id\":\"83YG\",\"pair\":\"NADXBT\",\
            \ \"state\":\"COMPLETE\",\"type\":\"BID\"}"
        (PrivateOrder
            {privateOrderBase = 568.7,
             privateOrderCounter = 3764.2,
             privateOrderCreationTimestamp = (posixSecondsToUTCTime 478873.467),
             privateOrderExpirationTimestamp = (posixSecondsToUTCTime 8768834.222),
             privateOrderFeeBase = 3687.3,
             privateOrderFeeCounter = 12.9,
             privateOrderLimitPrice = 765,
             privateOrderLimitVolume = 55.2,
             privateOrderId = "83YG",
             privateOrderPair = NADXBT,
             privateOrderState = COMPLETE,
             privateOrderOrderType = BID })
    it "PrivateOrders is parsed properly" $ do
      recordAesCheck
        "{\"orders\":[{\"base\":\"568.7\", \"counter\":3764.2,\"creation_timestamp\":478873467, \
            \ \"expiration_timestamp\":8768834222, \"fee_base\":\"3687.3\", \"fee_counter\":12.9,\
            \ \"limit_price\":765,\"limit_volume\":55.2,\"order_id\":\"83YG\",\"pair\":\"NADXBT\",\
            \ \"state\":\"COMPLETE\",\"type\":\"BID\"}]}"
        [privateOrderInner]
    it "OrderID is parsed properly" $ do
      recordAesCheck
        "{\"order_id\":\"57983\"}"
        ("57983" :: OrderID)
    it "PublicTrades is parsed properly" $ do
      recordAesCheck
        "{\"trades\":[{\"timestamp\":1431811395699,\"volume\":\"6754.09\",\"price\":\"5327.765\"}], \
            \ \"currency\":\"ZAR\"}"
         [tradeInner]
    it "RequestSuccess is parsed properly" $ do
      recordAesCheck
        "{\"success\":true}"
        (True :: RequestSuccess)
    it "PrivateOrderWithTrades is parsed properly" $ do
      recordAesCheck
        "{\"base\":\"568.7\", \"counter\":3764.2,\"creation_timestamp\":478873467, \
            \ \"expiration_timestamp\":8768834222, \"fee_base\":\"3687.3\", \"fee_counter\":12.9,\
            \ \"limit_price\":765,\"limit_volume\":55.2,\"order_id\":\"83YG\",\"pair\":\"NADXBT\",\
            \ \"state\":\"COMPLETE\",\"type\":\"BID\", \"trades\":[{\"timestamp\":1431811395699, \
            \ \"volume\":\"6754.09\",\"price\":\"5327.765\"}]}"
        (PrivateOrderWithTrades
            {privateOrderWithTradesBase = 568.7,
             privateOrderWithTradesCounter = 3764.2,
             privateOrderWithTradesCreationTimestamp = (posixSecondsToUTCTime 478873.467),
             privateOrderWithTradesExpirationTimestamp = (posixSecondsToUTCTime 8768834.222),
             privateOrderWithTradesFeeBase = 3687.3,
             privateOrderWithTradesFeeCounter = 12.9,
             privateOrderWithTradesLimitPrice = 765,
             privateOrderWithTradesLimitVolume = 55.2,
             privateOrderWithTradesId = "83YG",
             privateOrderWithTradesPair = NADXBT,
             privateOrderWithTradesState = COMPLETE,
             privateOrderWithTradesOrderType = BID,
             privateOrderWithTradesTrades = [tradeInner]} )
    it "WithdrawalRequest is parsed properly" $ do
      recordAesCheck
        "{\"status\":\"PENDING\", \"id\":\"7yrfU4987\"}"
        (WithdrawalRequest
            {withdrawalRequestStatus = PENDING,
             withdrawalRequestId = "7yrfU4987" })
    it "FundingAddress is parsed properly" $ do
      recordAesCheck
        "{\"asset\":\"ZAR\", \"address\":\"093gu959t894G\", \"total_received\":\"432.5\", \
            \ \"total_unconfirmed\":\"0.023\"}"
        (FundingAddress
            {fundingAddressAsset = ZAR,
             fundingAddressAddress = "093gu959t894G",
             fundingAddressTotalReceived = 432.5,
             fundingAddressTotalUnconfirmed = 0.023} )
    it "Transaction is parsed properly" $ do
      recordAesCheck
        "{\"row_index\":1,\"timestamp\":1387527013000,\"balance\":0.0199, \"available\":0.0299, \
            \ \"account_id\":\"3485527347968330182\", \"balance_delta\":0.0399, \
            \ \"available_delta\":0.0099,  \"currency\":\"XBT\",\"description\":\"Bought BTC 0.01 for R 79.00\"}"
        (Transaction
            {transactionRowIndex = 1,
             transactionTimestamp = (posixSecondsToUTCTime 1387527013),
             transactionBalance = 0.0199,
             transactionAvailable = 0.0299,
             transactionBalanceDelta = 0.0399,
             transactionAvailableDelta = 0.0099,
             transactionCurrency = XBT,
             transactionDescription = "Bought BTC 0.01 for R 79.00"} )
    it "Transactions is parsed properly" $ do
      recordAesCheck
        "{\"transactions\":[{\"row_index\":1,\"timestamp\":1387527013000,\"balance\":0.0199, \"available\":0.0299, \
            \ \"account_id\":\"3485527347968330182\", \"balance_delta\":0.0399, \
            \ \"available_delta\":0.0099,  \"currency\":\"XBT\",\"description\":\"Bought BTC 0.01 for R 79.00\"}]}"
        [transactionInner]

tickerInner :: Ticker
tickerInner =
    Ticker (posixSecondsToUTCTime 1431811395.699)
        3083.0
        3115.00
        3116.00
        19.776608
        XBTZAR

orderInner :: Order
orderInner =
    Order 654.98 3789.66

tradeInner :: Trade
tradeInner =
    Trade {
         tradeTimestamp = (posixSecondsToUTCTime 1431811395.699),
         tradeVolume = 6754.09,
         tradePrice = 5327.765 }

privateOrderInner :: PrivateOrder
privateOrderInner =
    PrivateOrder
        {privateOrderBase = 568.7,
        privateOrderCounter = 3764.2,
        privateOrderCreationTimestamp = (posixSecondsToUTCTime 478873.467),
        privateOrderExpirationTimestamp = (posixSecondsToUTCTime 8768834.222),
        privateOrderFeeBase = 3687.3,
        privateOrderFeeCounter = 12.9,
        privateOrderLimitPrice = 765,
        privateOrderLimitVolume = 55.2,
        privateOrderId = "83YG",
        privateOrderPair = NADXBT,
        privateOrderState = COMPLETE,
        privateOrderOrderType = BID }

transactionInner :: Transaction
transactionInner =
    Transaction
        {transactionRowIndex = 1,
         transactionTimestamp = (posixSecondsToUTCTime 1387527013),
         transactionBalance = 0.0199,
         transactionAvailable = 0.0299,
         transactionBalanceDelta = 0.0399,
         transactionAvailableDelta = 0.0099,
         transactionCurrency = XBT,
         transactionDescription = "Bought BTC 0.01 for R 79.00"}
