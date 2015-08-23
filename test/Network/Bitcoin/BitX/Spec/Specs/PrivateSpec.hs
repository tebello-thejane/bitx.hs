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
--import Debug.Trace
import Network.Bitcoin.BitX.Spec.Specs.NetSpec
--import Text.Show.Pretty (parseValue, valToStr)
import Safe

spec :: Spec
spec = describe "Private functionality test" $ do
    mauth <- runIO privateAuth
    if isJust mauth
        then do
            let auth = fromJust mauth
            it "getBalances connects to BitX and works" $
              connectsAndParsesOkay $ BitX.getBalances auth
            it "getAllOrders connects to BitX and works" $
              connectsAndParsesOkay $ BitX.getAllOrders auth Nothing Nothing
        else
            it "API key file not found -- skipping private tests" $
              True `shouldBe` True

connectsAndParsesOkay :: Show recd => IO (BitXAPIResponse recd) -> Bool
connectsAndParsesOkay = isValidResponse . unsafePerformIO
--connectsAndParsesOkay k = isValidResponse $ tracePretty (unsafePerformIO k)

--tracePretty :: Show a => a -> a
--tracePretty a = trace (show a) a

keyFileName :: FilePath
keyFileName = "PRIVATE_API_KEY"

privateAuth :: IO ( Maybe BitXAuth)
privateAuth =  do
    ex <- doesFileExist keyFileName
    if ex
        then do
            contents <- readFile keyFileName
            let lns = lines contents
            let key_id = headMay lns
            let key_secret = atMay lns 1

            if isJust key_id && isJust key_secret
                then
                    return . Just $
                        mkBitXAuth
                            & BitX.id .~ pack (fromJust key_id)
                            & BitX.secret .~ pack (fromJust key_secret)
                else
                    return Nothing
        else
            return Nothing
