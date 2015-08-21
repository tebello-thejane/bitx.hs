{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Network.Bitcoin.BitX.Spec.Specs.PrivateSpec
    (
    spec
    ) where

import Test.Hspec
import Network.Bitcoin.BitX.Types
import Network.Bitcoin.BitX.Public
import System.IO.Unsafe (unsafePerformIO)
import Network.Bitcoin.BitX.Response
import System.Directory

spec :: Spec
spec =
  describe "Private functionality test" $ do
    k <- runIO $ putStrLn ("Does it exist? " ++ (show privateFileExists))
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

privateFileExists :: Bool
privateFileExists = unsafePerformIO . doesFileExist $ "PRIVATE_API_KEY"

