-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Bitcoin.BitX.Public
-- Copyright   :  No Rights Reserved
-- License     :  Public Domain
--
-- Maintainer  :  Tebello Thejane <zyxoas+hackage@gmail.com>
-- Stability   :  Experimental
-- Portability :  non-portable (GHC Extensions)
--
-- =Usage example
--
-- As a small example, to get the current selling price of bitcoin on the BitX exchange, do the following:
--
-- @
--{-\# LANGUAGE QuasiQuotes \#-}
--
--import Record.Lens (view)
--import Record (lens)
--import Network.Bitcoin.BitX (BitXAPIResponse(..), getTicker, CcyPair(..))
--import Data.Text (unpack)
--import Network.HTTP.Types.Status (Status(..))
--import Network.HTTP.Conduit (responseStatus)
--
--main :: IO ()
--main = do
--  bitXResponse <- getTicker XBTZAR
--  case bitXResponse of
--    ValidResponse tic        -> print (view [lens| ask |] tic)
--    ErrorResponse err        ->
--        error $ "BitX error received: \"" ++ (unpack (view [lens| error |] err)) ++ "\""
--    ExceptionResponse ex     ->
--        error $ "Exception was thrown: \"" ++ (unpack ex) ++ "\""
--    UnparseableResponse resp ->
--        error $ "Bad HTTP response; HTTP status code was: \""
--                  ++ (show . statusCode . responseStatus $ resp) ++ "\""
-- @
--
-----------------------------------------------------------------------------

module Network.Bitcoin.BitX.Public
  (
    getTicker,
    getTickers,
    getOrderBook,
    getTrades,
    getLensTicker
  ) where

import Network.Bitcoin.BitX.Internal
import Network.Bitcoin.BitX.Types
import Network.Bitcoin.BitX.Response

{- | Returns the latest ticker indicators. -}

getTicker :: CcyPair -> IO (BitXAPIResponse Ticker)
getTicker cyp = simpleBitXGet_ $ "ticker?pair=" ++ show cyp

getLensTicker :: CcyPair -> IO (BitXAPIResponse LensTicker)
getLensTicker cyp = simpleBitXGet_ $ "ticker?pair=" ++ show cyp

{- | Returns the latest ticker indicators from all active BitX exchanges. -}

getTickers :: IO (BitXAPIResponse [Ticker])
getTickers = simpleBitXGet_ "tickers"

{- | Returns a list of bids and asks in the order book.

Ask orders are sorted by price ascending. Bid orders are sorted by price descending.
Note that multiple orders at the same price are not necessarily conflated. -}

getOrderBook :: CcyPair -> IO (BitXAPIResponse Orderbook)
getOrderBook cyp = simpleBitXGet_ $ "orderbook?pair=" ++ show cyp

{- | Returns a list of the most recent trades -}

getTrades :: CcyPair -> IO (BitXAPIResponse [Trade])
getTrades cyp = simpleBitXGet_ $ "trades?pair=" ++ show cyp

