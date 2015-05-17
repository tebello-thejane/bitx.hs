{-# LANGUAGE OverloadedStrings, QuasiQuotes, DataKinds, TemplateHaskell #-}

module Specs.AesonRecordSpec
    (
    spec
    ) where

import Test.Hspec
import Data.Aeson
import Network.Bitcoin.BitX.Types.Internal
import Network.Bitcoin.BitX.Types
import Record
import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock.POSIX

spec :: Spec
spec = do
  describe "FromJSON to Record" $ do
    it "BitXError is parsed properly" $ do
      recordAesCheck
        "{\"error\" : \"oops\", \"error_code\" : \"ABadError\"}"
        ([record| {error = "oops", errorCode = "ABadError"} |] :: BitXError)
    it "Ticker is parsed properly" $ do
      recordAesCheck
        "{\"timestamp\":1431811395699,\"bid\":\"3083.00\",\"ask\":\"3115.00\",\"last_trade\":\"3116.00\",\"rolling_24_hour_volume\":\"19.776608\",\"pair\":\"XBTZAR\"}"
        ([record|
            {ask = 3115.00,
             timestamp = (posixSecondsToUTCTime 1431811395.699),
             bid = 3083.0,
             rolling24HourVolume = 19.776608,
             lastTrade = 3116.00,
             pair = XBTZAR} |] :: Ticker)
    it "Balance is parsed properly" $ do
      recordAesCheck
        "{\"account_id\":\"314159\",\"asset\":\"ZAR\",\"balance\":\"2159.15\",\"reserved\":\"320\",\"unconfirmed\":\"175\"}"
        ([record|
            {accountID = "314159",
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

recordAesCheck :: (BitXAesRecordConvert rec aes, Show rec, Eq rec) => ByteString -> rec -> Expectation
recordAesCheck aesTxt recd = (fmap aesToRec $ decode aesTxt) `shouldBe` Just recd
