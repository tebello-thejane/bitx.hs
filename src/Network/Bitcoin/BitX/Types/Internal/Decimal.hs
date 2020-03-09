module Network.Bitcoin.BitX.Types.Internal.Decimal
    (
    realToDecimalByteString_
    )
where

import Data.ByteString (ByteString, stripSuffix)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Builder.Scientific
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (toStrict)
import Data.Scientific (Scientific)

realToDecimalByteString_ :: Scientific -> ByteString
realToDecimalByteString_ =
  handleInt . toStrict . toLazyByteString . formatScientificBuilder Fixed Nothing

handleInt :: ByteString -> ByteString
handleInt s = case stripSuffix (pack ".0") s of
  Nothing -> s
  Just s' -> s'

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
-- "0.0000001"
--
-- >>> realToDecimalByteString_ 0
-- "0"
--
-- >>> realToDecimalByteString_ 123.1234567
-- "123.1234567"
--
-- >>> realToDecimalByteString_ 0.00002683
-- "0.00002683"
