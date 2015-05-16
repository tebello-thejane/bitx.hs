{-# LANGUAGE OverloadedStrings, QuasiQuotes, DataKinds, TemplateHaskell #-}

module Specs.JsonSpec
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
    it "BitXError is parsed properly" $ do
      recordAesCheck
        "{\"timestamp\":1431811395699,\"bid\":\"3083.00\",\"ask\":\"3115.00\",\"last_trade\":\"3116.00\",\"rolling_24_hour_volume\":\"19.776608\",\"pair\":\"XBTZAR\"}"
        ([record|
            {ask = 3115.00,
             timestamp = (posixSecondsToUTCTime 1431811395.699),
             bid = 3083.0,
             rolling24HourVolume = 19.776608,
             lastTrade = 3116.00,
             pair = XBTZAR} |] :: Ticker)

recordAesCheck :: (BitXAesRecordConvert rec aes, Show rec, Eq rec) => ByteString -> rec -> Expectation
recordAesCheck aesTxt recd = (fmap aesToRec $ decode aesTxt) `shouldBe` Just recd
