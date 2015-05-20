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
-- The public BitX API.
--
-- Each one of the calls may either return a useful 'record', a 'BitXError' if BitX actually
-- returned an error, or 'Nothing' if some exception occured (or if the data returned by BitX was
-- unparseable).
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

{- | Returns the latest ticker indicators. -}

getTicker :: CcyPair -> IO (Maybe (Either BitXError Ticker))
getTicker cyp = simpleBitXGet_ $ "ticker?pair=" ++ show cyp

{- | Returns the latest ticker indicators from all active BitX exchanges. -}

getTickers :: IO (Maybe (Either BitXError [Ticker]))
getTickers = simpleBitXGet_ "tickers"

{- | Returns a list of bids and asks in the order book.

Ask orders are sorted by price ascending. Bid orders are sorted by price descending.
Note that multiple orders at the same price are not necessarily conflated. -}

getOrderBook :: CcyPair -> IO (Maybe (Either BitXError Orderbook))
getOrderBook cyp = simpleBitXGet_ $ "orderbook?pair=" ++ show cyp

{- | Returns a list of the most recent trades -}

getTrades :: CcyPair -> IO (Maybe (Either BitXError [Trade]))
getTrades cyp = simpleBitXGet_ $ "trades?pair=" ++ show cyp

