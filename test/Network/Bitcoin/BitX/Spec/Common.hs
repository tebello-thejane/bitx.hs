module Network.Bitcoin.BitX.Spec.Common
    (
    recordAesCheck
    ) where

import Test.Hspec
import Data.Aeson
import Network.Bitcoin.BitX (BitXAesRecordConvert(..))
import Data.ByteString.Lazy (ByteString)

recordAesCheck :: (BitXAesRecordConvert rec aes, Show rec, Eq rec) => ByteString -> rec -> Expectation
recordAesCheck aesTxt recd = (fmap aesToRec $ decode aesTxt) `shouldBe` Just recd
