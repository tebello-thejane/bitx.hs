{-# LANGUAGE OverloadedStrings, QuasiQuotes, DataKinds, TemplateHaskell #-}

module Network.Bitcoin.BitX.Spec.Specs.NetSpec
    (
    spec
    ) where

import Test.Hspec
import Network.Bitcoin.BitX.Types
import Network.Bitcoin.BitX.Public
import System.IO.Unsafe (unsafePerformIO)
import Network.Bitcoin.BitX.Response

spec :: Spec
spec = do
  describe "Public BitX connectivity" $ do
    it "getLensTicker connects to BitX and works" $ do
      connectsAndParsesOkay $ getLensTicker XBTZAR
    it "getTicker connects to BitX and works" $ do
      connectsAndParsesOkay $ getTicker XBTZAR
    it "getTickers connects to BitX and works" $ do
      connectsAndParsesOkay $ getTickers
    it "getOrderBook connects to BitX and works" $ do
      connectsAndParsesOkay $ getOrderBook XBTZAR
    it "getTrades connects to BitX and works" $ do
      connectsAndParsesOkay $ getTrades XBTKES

connectsAndParsesOkay :: IO (BitXAPIResponse rec) -> Bool
connectsAndParsesOkay = isValidResponse . unsafePerformIO

isValidResponse :: BitXAPIResponse rec -> Bool
isValidResponse (ValidResponse _) = True
isValidResponse        _          = False
