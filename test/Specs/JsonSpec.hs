{-# LANGUAGE OverloadedStrings #-}

module Specs.JsonSpec (spec) where

import Test.Hspec
import Data.Aeson
import Network.Bitcoin.BitX.Types.Internal

spec :: Spec
spec = do
  describe "strip" $ do
    it "BitXError is parsed properly " $ do
      ((decode "{\"error\" : \"oops\", \"error_code\" : \"ABadError\"}" ) :: Maybe BitXError_)
        `shouldBe` Just (BitXError_ "oops" "ABadError")
