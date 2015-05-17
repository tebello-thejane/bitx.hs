module Network.Bitcoin.BitX.Spec.Common
    (
    recordAesCheck,
    isLeft,
    isRight
    ) where

import Test.Hspec
import Data.Aeson
import Network.Bitcoin.BitX.Types.Internal
import Data.ByteString.Lazy (ByteString)

recordAesCheck :: (BitXAesRecordConvert rec aes, Show rec, Eq rec) => ByteString -> rec -> Expectation
recordAesCheck aesTxt recd = (fmap aesToRec $ decode aesTxt) `shouldBe` Just recd

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _        = False
