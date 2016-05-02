
import Lens.Micro ((^.))
import Network.Bitcoin.BitX (BitXAPIResponse(..), getTicker, CcyPair(..))
import qualified Network.Bitcoin.BitX as BitX
import Data.Text (unpack)
import Network.HTTP.Types.Status (Status(..))
import Network.HTTP.Client (responseStatus)

main :: IO ()
main = do
  bitXResponse <- getTicker XBTZAR
  case bitXResponse of
    ValidResponse tic        -> putStrLn ("1 bitcoin will set you back ZAR" ++ show (tic ^. BitX.ask) ++ ".00.")
    ErrorResponse err        ->
        error $ "BitX error received: \"" ++ unpack (err ^. BitX.error) ++ "\""
    ExceptionResponse ex     ->
        error $ "Exception was thrown: \"" ++ show ex ++ "\""
    UnparseableResponse _ resp ->
        error $ "Bad HTTP response; HTTP status code was: \"" ++ (show . statusCode . responseStatus $ resp) ++ "\""
