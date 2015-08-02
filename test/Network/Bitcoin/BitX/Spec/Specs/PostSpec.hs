{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Network.Bitcoin.BitX.Spec.Specs.PostSpec
    (
    spec
    ) where

import Test.Hspec
import Network.Bitcoin.BitX

spec :: Spec
spec =
  describe "PostEncode test" $ do
    it "OrderRequest is post-encoded properly" $
        postEncode
          OrderRequest
            {orderRequestPair = XBTZAR,
             orderRequestOrderType = BID,
             orderRequestVolume = 83.02,
             orderRequestPrice = 15.23 }
      `shouldBe`
        [("pair", "XBTZAR"),
         ("type", "BID"),
         ("volume", "83.02"),
         ("price", "15.23")]
    it "Asset is post-encoded properly" $
        postEncode ZAR
      `shouldBe`
        [("asset", "ZAR")]
    it "NewWithdrawal is post-encoded properly" $
        postEncode
          NewWithdrawal
            {newWithdrawalWithdrawalType = ZAR_EFT,
             newWithdrawalAmount = 83.02}
      `shouldBe`
        [("type", "ZAR_EFT"),
         ("amount", "83.02")]
    it "BitcoinSendRequest is post-encoded properly" $
        postEncode
          BitcoinSendRequest
            {bitcoinSendRequestAmount = 83.02,
             bitcoinSendRequestCurrency = XBT,
             bitcoinSendRequestAddress = "ahglk98aslfk",
             bitcoinSendRequestDescription = Just "Send some coinz to dis ere dude.",
             bitcoinSendRequestMessage = Just "Dude, ere'z your coinz."}
      `shouldBe`
        [("amount", "83.02"),
         ("currency", "XBT"),
         ("address", "ahglk98aslfk"),
         ("description", "Send some coinz to dis ere dude."),
         ("message", "Dude, ere'z your coinz.")]
    it "QuoteRequest is post-encoded properly" $
        postEncode
          QuoteRequest
            {quoteRequestQuoteType = BUY,
             quoteRequestPair = XBTKES,
             quoteRequestBaseAmount = 566.76}
      `shouldBe`
        [("type", "BUY"),
         ("pair", "XBTKES"),
         ("base_amount", "566.76")]
