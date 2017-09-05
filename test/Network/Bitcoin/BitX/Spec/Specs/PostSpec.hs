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
             orderRequestPrice = 15}
      `shouldBe`
        [("pair", "XBTZAR"),
         ("type", "BID"),
         ("volume", "83.02"),
         ("price", "15")]
    it "Asset is post-encoded properly" $
        postEncode ZAR
      `shouldBe`
        [("asset", "ZAR")]
    it "NewWithdrawal is post-encoded properly with beneficiary" $
        postEncode
          NewWithdrawal
            {newWithdrawalWithdrawalType = ZAR_EFT,
             newWithdrawalAmount = 83.02,
             newWithdrawalBeneficiaryId = Just "Some guy"}
      `shouldBe`
        [("type", "ZAR_EFT"),
         ("amount", "83.02"),
         ("beneficiary_id","Some guy")]
    it "NewWithdrawal is post-encoded properly with no beneficiary" $
        postEncode
          NewWithdrawal
            {newWithdrawalWithdrawalType = ZAR_EFT,
             newWithdrawalAmount = 83.02,
             newWithdrawalBeneficiaryId = Nothing}
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
             quoteRequestPair = XBTMYR,
             quoteRequestBaseAmount = 566.76}
      `shouldBe`
        [("type", "BUY"),
         ("pair", "XBTMYR"),
         ("base_amount", "566.76")]
    it "MarketOrderRequest for buy is post-encoded properly" $
        postEncode
          MarketOrderRequest
            {marketOrderRequestPair = XBTZAR,
             marketOrderRequestOrderType = BID,
             marketOrderRequestVolume = 15.023}
     `shouldBe`
       [("type", "BUY"),
        ("pair", "XBTZAR"),
        ("counter_volume", "15.023")]
    it "MarketOrderRequest for sell is post-encoded properly" $
        postEncode
          MarketOrderRequest
            {marketOrderRequestPair = XBTZAR,
             marketOrderRequestOrderType = ASK,
             marketOrderRequestVolume = 15.023}
     `shouldBe`
       [("type", "SELL"),
        ("pair", "XBTZAR"),
        ("base_volume", "15.023")]
