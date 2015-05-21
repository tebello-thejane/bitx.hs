{-# LANGUAGE OverloadedStrings, QuasiQuotes, DataKinds, TemplateHaskell #-}

module Network.Bitcoin.BitX.Spec.Specs.AesonRecordSpec
    (
    spec
    ) where

import Test.Hspec
import Network.Bitcoin.BitX.Types
import Record
import Data.Time.Clock.POSIX
import Network.Bitcoin.BitX.Spec.Common

spec :: Spec
spec = do
  describe "FromJSON to Record" $ do
    it "BitXError is parsed properly" $ do
      recordAesCheck
        "{\"error\" : \"oops\", \"error_code\" : \"ABadError\"}"
        ([record| {error = "oops", errorCode = "ABadError"} |] :: BitXError)
    it "Ticker is parsed properly" $ do
      recordAesCheck
        "{\"timestamp\":1431811395699,\"bid\":\"3083.00\",\"ask\":\"3115.00\",\
            \\"last_trade\":\"3116.00\",\"rolling_24_hour_volume\":\"19.776608\",\"pair\":\"XBTZAR\"}"
        ([record|
            {ask = 3115.00,
             timestamp = (posixSecondsToUTCTime 1431811395.699),
             bid = 3083.0,
             rolling24HourVolume = 19.776608,
             lastTrade = 3116.00,
             pair = XBTZAR} |] :: Ticker)
    it "Balance is parsed properly" $ do
      recordAesCheck
        "{\"account_id\":\"314159\",\"asset\":\"ZAR\",\"balance\":\"2159.15\",\"reserved\":\"320\",\
            \\"unconfirmed\":\"175\"}"
        ([record|
            {id = "314159",
             asset = ZAR,
             balance = 2159.15,
             reserved = 320,
             unconfirmed = 175} |] :: Balance)
    it "Order is parsed properly" $ do
      recordAesCheck
        "{\"volume\":\"314159\",\"price\":\"4321\"}"
        ([record|
            {volume = 314159,
             price = 4321} |] :: Order)
    it "WithdrawalRequest is parsed properly" $ do
      recordAesCheck
        "{\"status\":\"PENDING\",\"id\":\"271828\"}"
        ([record|
            {status = PENDING,
            id = "271828" } |] :: WithdrawalRequest)
    it "Tickers is parsed properly" $ do
      recordAesCheck
        "{\"tickers\":[{\"timestamp\":1431811395699,\"bid\":\"3083.00\",\"ask\":\"3115.00\",\
            \\"last_trade\":\"3116.00\",\"rolling_24_hour_volume\":\"19.776608\",\
            \\"pair\":\"XBTZAR\"}]}"
        [tickerInner]
    it "Orderbook is parsed properly" $ do
      recordAesCheck
        "{\"timestamp\":1431811395699,\"bids\":[{\"volume\":\"654.98\",\"price\":\"3789.66\"}],\
            \\"asks\":[{\"volume\":\"654.98\",\"price\":\"3789.66\"}]}"
        ([record|
            {timestamp = (posixSecondsToUTCTime 1431811395.699),
             bids = [orderInner],
             asks = [orderInner] } |] :: Orderbook)
    it "Trade is parsed properly" $ do
      recordAesCheck
        "{\"timestamp\":1431811395699,\"volume\":\"6754.09\",\"price\":\"5327.765\"}"
        ([record|
            {timestamp = (posixSecondsToUTCTime 1431811395.699),
             volume = 6754.09,
             price = 5327.765 } |] :: Trade)
    it "PublicTrades is parsed properly" $ do
      recordAesCheck
        "{\"trades\":[{\"timestamp\":1431811395699,\"volume\":\"6754.09\",\
            \\"price\":\"5327.765\"}],\"currency\":\"ZAR\"}"
        [tradeInner]
    it "PrivateOrder is parsed properly" $ do
      recordAesCheck
        "{\"base\":\"568.7\", \"counter\":3764.2,\"creation_timestamp\":478873467, \
            \\"expiration_timestamp\":8768834222, \"fee_base\":\"3687.3\", \"fee_counter\":12.9,\
            \\"limit_price\":765,\"limit_volume\":55.2,\"order_id\":\"83YG\",\"pair\":\"NADXBT\",\
            \\"state\":\"COMPLETE\",\"type\":\"BID\"}"
        ([record|
            {base = 568.7,
             counter = 3764.2,
             creationTimestamp = (posixSecondsToUTCTime 478873.467),
             expirationTimestamp = (posixSecondsToUTCTime 8768834.222),
             feeBase = 3687.3,
             feeCounter = 12.9,
             limitPrice = 765,
             limitVolume = 55.2,
             id = "83YG",
             pair = NADXBT,
             state = COMPLETE,
             type = BID } |] :: PrivateOrder)
    it "PrivateOrders is parsed properly" $ do
      recordAesCheck
        "{\"orders\":[{\"base\":\"568.7\", \"counter\":3764.2,\"creation_timestamp\":478873467, \
            \\"expiration_timestamp\":8768834222, \"fee_base\":\"3687.3\", \"fee_counter\":12.9,\
            \\"limit_price\":765,\"limit_volume\":55.2,\"order_id\":\"83YG\",\"pair\":\"NADXBT\",\
            \\"state\":\"COMPLETE\",\"type\":\"BID\"}]}"
        [privateOrderInner]
    it "OrderID is parsed properly" $ do
      recordAesCheck
        "{\"order_id\":\"57983\"}"
        ("57983" :: OrderID)
    it "PublicTrades is parsed properly" $ do
      recordAesCheck
        "{\"trades\":[{\"timestamp\":1431811395699,\"volume\":\"6754.09\",\"price\":\"5327.765\"}]}, \
            \\"currency\":\"ZAR\"}"
         [tradeInner]
    it "RequestSuccess is parsed properly" $ do
      recordAesCheck
        "{\"success\":true}"
        (True :: RequestSuccess)
    it "PrivateOrderWithTrades is parsed properly" $ do
      recordAesCheck
        "{\"base\":\"568.7\", \"counter\":3764.2,\"creation_timestamp\":478873467, \
            \\"expiration_timestamp\":8768834222, \"fee_base\":\"3687.3\", \"fee_counter\":12.9,\
            \\"limit_price\":765,\"limit_volume\":55.2,\"order_id\":\"83YG\",\"pair\":\"NADXBT\",\
            \\"state\":\"COMPLETE\",\"type\":\"BID\"}, \"trades\":[{\"timestamp\":1431811395699, \
            \\"volume\":\"6754.09\",\"price\":\"5327.765\"}]"
        ([record|
            {base = 568.7,
             counter = 3764.2,
             creationTimestamp = (posixSecondsToUTCTime 478873.467),
             expirationTimestamp = (posixSecondsToUTCTime 8768834.222),
             feeBase = 3687.3,
             feeCounter = 12.9,
             limitPrice = 765,
             limitVolume = 55.2,
             id = "83YG",
             pair = NADXBT,
             state = COMPLETE,
             type = BID,
             trades = [tradeInner]} |] :: PrivateOrderWithTrades)

tickerInner :: Ticker
tickerInner =
    [record|
        {ask = 3115.00,
        timestamp = (posixSecondsToUTCTime 1431811395.699),
        bid = 3083.0,
        rolling24HourVolume = 19.776608,
        lastTrade = 3116.00,
        pair = XBTZAR} |]

orderInner :: Order
orderInner =
    [record|
        {volume = 654.98,
         price = 3789.66} |]

tradeInner :: Trade
tradeInner =
    [record|
        {timestamp = (posixSecondsToUTCTime 1431811395.699),
         volume = 6754.09,
         price = 5327.765 } |]

privateOrderInner :: PrivateOrder
privateOrderInner =
    [record|
        {base = 568.7,
        counter = 3764.2,
        creationTimestamp = (posixSecondsToUTCTime 478873.467),
        expirationTimestamp = (posixSecondsToUTCTime 8768834.222),
        feeBase = 3687.3,
        feeCounter = 12.9,
        limitPrice = 765,
        limitVolume = 55.2,
        id = "83YG",
        pair = NADXBT,
        state = COMPLETE,
        type = BID } |]


