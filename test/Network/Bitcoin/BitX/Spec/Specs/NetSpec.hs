{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Network.Bitcoin.BitX.Spec.Specs.NetSpec
    (
    spec,
    isValidResponse
    ) where

import Test.Hspec
import Network.Bitcoin.BitX.Types
import Network.Bitcoin.BitX.Public
import System.IO.Unsafe (unsafePerformIO)
import Network.Bitcoin.BitX.Response

spec :: Spec
spec =
  describe "Public BitX connectivity" $ do
    it "getTicker connects to BitX and works" $
      connectsAndParsesOkay $ getTicker XBTZAR
    it "getTickers connects to BitX and works" $
      connectsAndParsesOkay getTickers
    it "getOrderBook connects to BitX and works" $
      connectsAndParsesOkay $ getOrderBook XBTZAR
    it "getTrades connects to BitX and works" $
      connectsAndParsesOkay $ getTrades XBTKES

connectsAndParsesOkay :: IO (BitXAPIResponse recd) -> Bool
connectsAndParsesOkay = isValidResponse . unsafePerformIO

isValidResponse :: BitXAPIResponse recd -> Bool
isValidResponse (ValidResponse _) = True
isValidResponse        _          = False
