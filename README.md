[![Build Status](https://travis-ci.org/tebello-thejane/bitx-haskell.svg?branch=master)](https://travis-ci.org/tebello-thejane/bitx-haskell)
[![Hackage](https://budueba.com/hackage/bitx-bitcoin)](https://hackage.haskell.org/package/bitx-bitcoin)
[![CC0 Public Domain](http://b.repl.ca/v1/CC0-Public_Domain-brightgreen.png)](http://creativecommons.org/publicdomain/zero/1.0/)

(Hopefully useful) Haskell bindings to the [BitX](https://bitx.co/) bitcoin exchange's [API](https://bitx.co/api).

As a minimal example, to get the current selling price (in South African Rand) of bitcoin on the
BitX exchange, do the following:

```haskell
{-# LANGUAGE DataKinds #-}

import Control.Lens ((^.))
import Network.Bitcoin.BitX (BitXAPIResponse(..), getTicker, CcyPair(..))
import qualified Network.Bitcoin.BitX as BitX
import Data.Text (unpack)
import Network.HTTP.Types.Status (Status(..))
import Network.HTTP.Conduit (responseStatus)

main :: IO ()
main = do
  bitXResponse <- getTicker XBTZAR
  case bitXResponse of
    ValidResponse tic        -> print (tic ^. BitX.ask)
    ErrorResponse err        ->
        error $ "BitX error received: \"" ++ unpack (err ^. BitX.error) ++ "\""
    ExceptionResponse ex     ->
        error $ "Exception was thrown: \"" ++ show ex ++ "\""
    UnparseableResponse resp ->
        error $ "Bad HTTP response; HTTP status code was: \"" ++ (show . statusCode . responseStatus $ resp) ++ "\""
```

Note that the code snippet above depends on [http-types](https://hackage.haskell.org/package/http-types),
[http-conduit](https://hackage.haskell.org/package/http-conduit), [lens](https://hackage.haskell.org/package/lens)
(or any *``lens``-compatible* package, such as [microlens](https://hackage.haskell.org/package/microlens)),
and finally **bitx-bitcoin**.

This library is known to work on Windows, but if you wish to use it then you will have to do a bit
more work due to the ``Network`` library not building on Windows out of the box. See
[this blog post by Neil Mitchell](http://neilmitchell.blogspot.com/2010/12/installing-haskell-network-library-on.html).
