[![Build Status](https://travis-ci.org/tebello-thejane/bitx-haskell.svg?branch=master)](https://travis-ci.org/tebello-thejane/bitx-haskell)
[![Hackage](https://budueba.com/hackage/bitx-bitcoin)](https://hackage.haskell.org/package/bitx-bitcoin)

(Hopefully useful) Haskell bindings to the BitX bitcoin exchange's API.

As a minimal example, to get the current selling price (in South African Rand) of bitcoin on the BitX exchange, do the following:

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Record.Lens
import Record
import Network.Bitcoin.BitX
import Data.Text

main = do
  bitXResponse <- getTicker XBTZAR
  case bitXResponse of
    ValidResponse tic        -> print (view [lens| ask |] tic)
    ErrorResponse err        -> 
        error $ "BitX error received: \"" ++ (unpack (view [lens| error |] err)) ++ "\""
    ExceptionResponse ex     -> 
        error $ "Exception was thrown: \"" ++ (unpack ex) ++ "\""
    UnparseableResponse resp -> 
        error $ "Bad HTTP response; HTTP status code was: \"" ++ (show . statusCode $ resp) ++ "\""
```
