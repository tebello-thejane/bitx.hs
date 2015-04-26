module Network.Bitcoin.BitX.Public
  (
  ) where

{-# LANGUAGE DeriveGeneric #-}

import qualified Data.Aeson (FromJSON, ToJSON, decode, encode) as Aeson
import Network.Bitcoin.BitX.Types

{- | Returns the latest ticker indicators. -}

getTicker :: CcyPair -> IO Ticker
getTicker = undefined

{- | Returns the latest ticker indicators from all active BitX exchanges. -}

getTickers :: IO [Ticker]
getTickers = undefined

{- | Returns a list of bids and asks in the order book.

Ask orders are sorted by price ascending. Bid orders are sorted by price descending.
Note that multiple orders at the same price are not necessarily conflated. -}

getOrderBook :: CcyPair -> IO Orderbook
getOrderBook = undefined

{- | Returns a list of the most recent trades -}

getTrades :: CcyPair -> IO [Trade]
getTrades = undefined

