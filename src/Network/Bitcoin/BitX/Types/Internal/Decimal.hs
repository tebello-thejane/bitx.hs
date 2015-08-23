
module Network.Bitcoin.BitX.Types.Internal.Decimal
    (
    realToDecimalByteString_
    )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

import Numeric (showFFloat)

realToDecimalByteString_ :: (RealFrac a) => a -> ByteString
realToDecimalByteString_ k =
    pack
    . handleIntegers
    . reverse . dropWhile (== '0') . reverse
    $ (showFFloat Nothing . (fromRational :: Rational -> Double)
    . toRational $ truncate6 k) ""

truncate6 :: RealFrac a => a -> Double
truncate6 k =
    (/ (1000 * 1000 :: Double)) . fromInteger
    $ truncate (k * 1000 * 1000)

handleIntegers :: String -> String
handleIntegers x =
    if last x == '.'
        then init x
        else x

-- |
-- >>> realToDecimalByteString_ 3
-- "3"
--
-- >>> realToDecimalByteString_ 123456789
-- "123456789"
--
-- >>> realToDecimalByteString_ 100
-- "100"
--
-- >>> realToDecimalByteString_ 0.3
-- "0.3"
--
-- >>> realToDecimalByteString_ 0.12
-- "0.12"
--
-- >>> realToDecimalByteString_ 10.1
-- "10.1"
--
-- >>> realToDecimalByteString_ 3.0
-- "3"
--
-- >>> realToDecimalByteString_ 0.001
-- "0.001"
--
-- >>> realToDecimalByteString_ 0.000001
-- "0.000001"
--
-- >>> realToDecimalByteString_ 0.0000001
-- "0"
--
-- >>> realToDecimalByteString_ 0
-- "0"
--
-- >>> realToDecimalByteString_ 123.1234567
-- "123.123456"
--
