{-# LANGUAGE OverloadedStrings, QuasiQuotes, DataKinds, TemplateHaskell #-}

module Network.Bitcoin.BitX.Spec.Specs.PostSpec
    (
    spec
    ) where

import Test.Hspec
import Network.Bitcoin.BitX
import Record

spec :: Spec
spec = do
  describe "PostEncode test" $ do
    it "OrderRequest is post-encoded properly" $ do
        postEncode
          ([record|
            {pair = XBTZAR,
             type = BID,
             volume = 83.02,
             price = 15.23 } |] :: OrderRequest)
      `shouldBe`
        [("pair", "XBTZAR"),
         ("type", "BID"),
         ("volume", "83.02"),
         ("price", "15.23")]
    it "Asset is post-encoded properly" $ do
        postEncode ZAR
      `shouldBe`
        [("asset", "ZAR")]
    it "NewWithdrawal is post-encoded properly" $ do
        postEncode
          ([record|
            {type = ZAR_EFT,
             amount = 83.02} |] :: NewWithdrawal)
      `shouldBe`
        [("type", "ZAR_EFT"),
         ("amount", "83.02")]
    it "BitcoinSendRequest is post-encoded properly" $ do
        postEncode
          ([record|
            {amount = 83.02,
             currency = XBT,
             address = "ahglk98aslfk",
             description = Just "Send some coinz to dis ere dude.",
             message = Just "Dude, ere'z your coinz."} |] :: BitcoinSendRequest)
      `shouldBe`
        [("amount", "83.02"),
         ("currency", "XBT"),
         ("address", "ahglk98aslfk"),
         ("description", "Send some coinz to dis ere dude."),
         ("message", "Dude, ere'z your coinz.")]
    it "QuoteRequest is post-encoded properly" $ do
        postEncode
          ([record|
            {type = BUY,
             pair = XBTKES,
             baseAmount = 566.76} |] :: QuoteRequest)
      `shouldBe`
        [("type", "BUY"),
         ("pair", "XBTKES"),
         ("base_amount", "566.76")]
