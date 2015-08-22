{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Network.Bitcoin.BitX.Spec.Specs.PrivateSpec
    (
    Network.Bitcoin.BitX.Spec.Specs.PrivateSpec.spec
    ) where

import Test.Hspec
import Network.Bitcoin.BitX.Types
import qualified Network.Bitcoin.BitX as BitX
import System.IO.Unsafe (unsafePerformIO)
import Network.Bitcoin.BitX.Response
import System.Directory (doesFileExist)
import Data.Maybe (isJust, fromJust)
import Data.Text (pack)
import Lens.Micro
import Debug.Trace
import Network.Bitcoin.BitX.Spec.Specs.NetSpec
import Text.Show.Pretty

spec :: Spec
spec =
  describe "Private functionality test" $ do
    mauth <- runIO privateAuth
    if isJust mauth
        then do
            let auth = fromJust mauth
            it "getBalances connects to BitX and works" $
              connectsAndParsesOkay $ BitX.getBalances auth
        else do
            it "API key file not found -- skipping private tests" $
              True `shouldBe` True

connectsAndParsesOkay :: Show recd => IO (BitXAPIResponse recd) -> Bool
connectsAndParsesOkay k = isValidResponse $ tracePretty (unsafePerformIO k)

tracePretty a = trace (valToStr . fromJust . parseValue $ show a) a

keyFileName :: FilePath
keyFileName = "PRIVATE_API_KEY"

privateAuth :: IO ( Maybe BitXAuth)
privateAuth =  do
    ex <- doesFileExist keyFileName
    if ex
        then do
            contents <- readFile keyFileName
            let lns = lines contents
            return . Just $
                mkBitXAuth
                    & BitX.id .~ pack (head lns)
                    & BitX.secret .~ pack (lns !! 1)
        else
            return Nothing
