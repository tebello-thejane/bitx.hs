{-# LANGUAGE OverloadedStrings, QuasiQuotes, DataKinds, TemplateHaskell #-}

module Network.Bitcoin.BitX.Spec.Specs.JsonSpec
    (
    spec
    ) where

import Test.Hspec
import Network.Bitcoin.BitX.Types
import Record
import Network.Bitcoin.BitX.Spec.Common
import Data.Time.Clock.POSIX

spec :: Spec
spec = do
  describe "FromJSON intances" $ do
    it "QuotedDecimal is parsed whether quoted or not, for floating point numbers" $ do
      recordAesCheck
        "{\"volume\":314159.23,\"price\":\"4321.56\"}"
        ([record|
            {volume = 314159.23,
             price = 4321.56} |] :: Order)
    it "QuotedDecimal is parsed whether quoted or not, for integral numbers" $ do
      recordAesCheck
        "{\"volume\":314159,\"price\":\"4321\"}"
        ([record|
            {volume = 314159,
             price = 4321} |] :: Order)
    it "OrderType BUY is parsed properly" $ do
      recordAesCheck
        "{\"base\":\"568.7\", \"counter\":3764.2,\"creation_timestamp\":478873467, \
            \\"expiration_timestamp\":8768834222, \"fee_base\":\"3687.3\", \"fee_counter\":12.9,\
            \\"limit_price\":765,\"limit_volume\":55.2,\"order_id\":\"83YG\",\"pair\":\"NADXBT\",\
            \\"state\":\"COMPLETE\",\"type\":\"BUY\"}"
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
    it "OrderType BID is parsed properly" $ do
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
    it "OrderType ASK is parsed properly" $ do
      recordAesCheck
        "{\"base\":\"568.7\", \"counter\":3764.2,\"creation_timestamp\":478873467, \
            \\"expiration_timestamp\":8768834222, \"fee_base\":\"3687.3\", \"fee_counter\":12.9,\
            \\"limit_price\":765,\"limit_volume\":55.2,\"order_id\":\"83YG\",\"pair\":\"NADXBT\",\
            \\"state\":\"COMPLETE\",\"type\":\"ASK\"}"
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
             type = ASK } |] :: PrivateOrder)
    it "OrderType SELL is parsed properly" $ do
      recordAesCheck
        "{\"base\":\"568.7\", \"counter\":3764.2,\"creation_timestamp\":478873467, \
            \\"expiration_timestamp\":8768834222, \"fee_base\":\"3687.3\", \"fee_counter\":12.9,\
            \\"limit_price\":765,\"limit_volume\":55.2,\"order_id\":\"83YG\",\"pair\":\"NADXBT\",\
            \\"state\":\"COMPLETE\",\"type\":\"SELL\"}"
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
             type = ASK } |] :: PrivateOrder)
