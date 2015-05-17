{-# LANGUAGE OverloadedStrings, QuasiQuotes, DataKinds, TemplateHaskell #-}

module Network.Bitcoin.BitX.Spec.Specs.NetSpec
    (
    spec
    ) where

import Test.Hspec
import Network.Bitcoin.BitX.Types
import Network.Bitcoin.BitX.Spec.Common
import Network.Bitcoin.BitX.Public
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromJust)
--import Data.Time.Clock.POSIX

spec :: Spec
spec = do
  describe "Public BitX connectivity" $ do
    it "getTicker connects to BitX and works" $ do
      connectsAndParsesOkay $ getTicker XBTZAR
    it "getTickers connects to BitX and works" $ do
      connectsAndParsesOkay $ getTickers
    it "getOrderBook connects to BitX and works" $ do
      connectsAndParsesOkay $ getOrderBook XBTZAR
    it "getTrades connects to BitX and works" $ do
      connectsAndParsesOkay $ getTrades XBTZAR

connectsAndParsesOkay :: IO (Maybe (Either a b)) -> Bool
connectsAndParsesOkay = isRight . fromJust . unsafePerformIO
