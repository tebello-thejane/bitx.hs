[![Build Status](https://travis-ci.org/tebello-thejane/bitx-haskell.svg?branch=master)](https://travis-ci.org/tebello-thejane/bitx-haskell)
[![Hackage](https://budueba.com/hackage/bitx-bitcoin)](https://hackage.haskell.org/package/bitx-bitcoin)
[![CC0 Public Domain](http://b.repl.ca/v1/CC0-Public_Domain-brightgreen.png)](http://creativecommons.org/publicdomain/zero/1.0/)

(Hopefully useful) Haskell bindings to the [BitX](https://bitx.co/) bitcoin exchange's [API](https://bitx.co/api).

As a minimal example, to get the current selling price (in South African Rand) of bitcoin on the BitX exchange, do the following:

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Record.Lens (view)
import Record (lens)
import Network.Bitcoin.BitX (BitXAPIResponse(..), getTicker, CcyPair(..))
import Data.Text (unpack)
import Network.HTTP.Types.Status (Status(..))
import Network.HTTP.Conduit (responseStatus)

main :: IO ()
main = do
  bitXResponse <- getTicker XBTZAR
  case bitXResponse of
    ValidResponse tic        -> print (view [lens| ask |] tic)
    ErrorResponse err        ->
        error $ "BitX error received: \"" ++ (unpack (view [lens| error |] err)) ++ "\""
    ExceptionResponse ex     ->
        error $ "Exception was thrown: \"" ++ (unpack ex) ++ "\""
    UnparseableResponse resp ->
        error $ "Bad HTTP response; HTTP status code was: \"" ++ (show . statusCode . responseStatus $ resp) ++ "\""
```

Note that the code snippet above depends on [http-types](https://hackage.haskell.org/package/http-types), [http-conduit](https://hackage.haskell.org/package/http-conduit), [record](https://hackage.haskell.org/package/record), and finally **bitx-bitcoin**.

Note that this library **will not** build on Windows currently, due to networking dependencies which have no Windows support.
