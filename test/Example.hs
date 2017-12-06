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
    ValidResponse tic        ->
      case tic ^. BitX.ask of
        Nothing              -> putStrLn "The BTC-ZAR exchange not currently have an ask price..."
        Just p               -> putStrLn ("1 bitcoin will set you back ZAR" ++ show p ++ ".")
    ErrorResponse err        ->
        error $ "BitX error received: \"" ++ unpack (err ^. BitX.error) ++ "\""
    ExceptionResponse ex     ->
        error $ "Exception was thrown: \"" ++ show ex ++ "\""
    UnparseableResponse _ resp ->
        error $ "Bad HTTP response; HTTP status code was: \"" ++ (show . statusCode . responseStatus $ resp) ++ "\""
