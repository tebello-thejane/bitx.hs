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
--import Data.Time.Clock.POSIX

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

recordAesCheck :: (BitXAesRecordConvert rec aes, Show rec, Eq rec) => ByteString -> rec -> Expectation
recordAesCheck aesTxt recd = (fmap aesToRec $ decode aesTxt) `shouldBe` Just recd
