module Network.Bitcoin.BitX.Response
  (
    BitXAPIResponse(..)
  ) where


import Network.HTTP.Conduit (Response(..))
import Data.ByteString.Lazy (ByteString)
import Network.Bitcoin.BitX.Types
import Data.Text (Text)

data BitXAPIResponse rec =
      ExceptionResponse Text
    | ErrorResponse BitXError
    | ValidResponse rec
    | UnparseableResponse (Response ByteString)
    deriving (Show, Eq)
