{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Bitcoin.BitX.Public
-- Copyright   :  2016 Tebello Thejane
-- License     :  BSD3
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
--import Control.Lens ((^.))
--import Network.Bitcoin.BitX (BitXAPIResponse(..), getTicker, CcyPair(..))
--import qualified Network.Bitcoin.BitX as BitX
--import Data.Text (unpack)
--import Network.HTTP.Types.Status (Status(..))
--import Network.HTTP.Client (responseStatus)
--
--main :: IO ()
--main = do
--  bitXResponse <- getTicker XBTZAR
--  case bitXResponse of
--    ValidResponse tic
--      case tic ^. BitX.ask of
--        Nothing              ->  putStrLn "The BTC-ZAR exchange not currently have an ask price..."
--        Just p               ->  putStrLn ("1 bitcoin will set you back ZAR" ++ show p ++ ".00.")
--    ErrorResponse err        ->
--        error $ "BitX error received: \\"" ++ unpack (err ^. BitX.error) ++ "\\""
--    ExceptionResponse ex     ->
--        error $ "Exception was thrown: \\"" ++ show ex ++ "\\""
--    UnparseableResponse _ resp ->
--        error $ "Bad HTTP response; HTTP status code was: \\"" ++ (show . statusCode . responseStatus $ resp) ++ "\\""
-- @
--
-----------------------------------------------------------------------------

module Network.Bitcoin.BitX.Public
  (
    getTicker,
    getTickers,
    getOrderBook,
    getTrades
  ) where

import Network.Bitcoin.BitX.Internal
import Network.Bitcoin.BitX.Types
import Network.Bitcoin.BitX.Types.Internal
import Network.Bitcoin.BitX.Response
import Data.Time.Clock (UTCTime)
import Data.Text (pack)
import Data.Monoid ((<>))

{- | Returns the latest ticker indicators. -}

getTicker :: CcyPair -> IO (BitXAPIResponse Ticker)
getTicker cyp = simpleBitXGet_ $ "ticker?pair=" <> pack (show cyp)

{- | Returns the latest ticker indicators from all active BitX exchanges. -}

getTickers :: IO (BitXAPIResponse [Ticker])
getTickers = simpleBitXGet_ "tickers"

{- | Returns a list of bids and asks in the order book.

Ask orders are sorted by price ascending. Bid orders are sorted by price descending.
Note that multiple orders at the same price are not necessarily conflated. -}

getOrderBook :: CcyPair -> IO (BitXAPIResponse Orderbook)
getOrderBook cyp = simpleBitXGet_ $ "orderbook?pair=" <> pack (show cyp)

{- | Returns a list of the most recent trades -}

getTrades :: Maybe UTCTime -> CcyPair -> IO (BitXAPIResponse [Trade])
getTrades   Nothing    cyp = simpleBitXGet_ $ "trades?pair=" <> pack (show cyp)
getTrades (Just since) cyp = simpleBitXGet_ $ "trades?pair=" <> pack (show cyp) <> "&since=" <> pack (show (timeToTimestamp since))

