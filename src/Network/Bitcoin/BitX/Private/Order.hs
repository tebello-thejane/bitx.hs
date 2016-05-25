{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Bitcoin.BitX.Private.Order
-- Copyright   :  2016 Tebello Thejane
-- License     :  BSD3
--
-- Maintainer  :  Tebello Thejane <zyxoas+hackage@gmail.com>
-- Stability   :  Experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Creating and working with orders
--
-- Trading on the market is done by submitting trade orders. After a new order has been created,
-- it is submitted for processing by the order matching engine. The order then either matches
-- against an existing order in the order book and is filled or it rests in the order book until it
-- is stopped.
--
-----------------------------------------------------------------------------

module Network.Bitcoin.BitX.Private.Order
  (
  getAllOrders,
  postOrder,
  stopOrder,
  getOrder,
  postMarketOrder
  ) where

import Network.Bitcoin.BitX.Internal
import Network.Bitcoin.BitX.Types
import qualified Data.Text as Txt
import Network.Bitcoin.BitX.Response

{- | Returns a list of the most recently placed orders.

If the second parameter is @Nothing@ then this will return orders for all markets, whereas if it is
@Just cpy@ for some @CcyPair cpy@ then the results will be specific to that market.

If the third parameter is @Nothing@ then this will return orders in all states, whereas if it is
@Just COMPLETE@ or @Just PENDING@ then it will return only completed or pending orders, respectively.

This list is truncated after 100 items.

@Perm_R_Orders@ permission is required.
 -}

getAllOrders :: BitXAuth -> Maybe CcyPair -> Maybe RequestStatus -> IO (BitXAPIResponse [PrivateOrder])
getAllOrders auth cpair stat = simpleBitXGetAuth_ auth url
    where
        url = "listorders" ++ case (cpair, stat) of
            (Nothing, Nothing)  -> ""
            (Just pr, Nothing)  -> "?pair=" ++ show pr
            (Nothing, Just st)  -> "?state=" ++ show st
            (Just pr, Just st)  -> "?pair=" ++ show pr ++ "&state=" ++ show st

{- | Create a new order.

__Warning! Orders cannot be reversed once they have executed. Please ensure your program has been__
__thoroughly tested before submitting orders.__

@Perm_W_Orders@ permission is required.
 -}

postOrder :: BitXAuth -> OrderRequest -> IO (BitXAPIResponse OrderID)
postOrder auth oreq = simpleBitXPOSTAuth_ auth oreq "postorder"

{- | Request to stop an order.

@Perm_W_Orders@ permission is required.
 -}

stopOrder :: BitXAuth -> OrderID -> IO (BitXAPIResponse RequestSuccess)
stopOrder auth oid = simpleBitXPOSTAuth_ auth oid "stoporder"

{- | Get an order by its ID

@Perm_R_Orders@ permission is required.
 -}

getOrder :: BitXAuth -> OrderID -> IO (BitXAPIResponse PrivateOrderWithTrades)
getOrder auth oid = simpleBitXGetAuth_ auth $ "orders/" ++ Txt.unpack oid

{- | Create a new market order.

__Warning! Orders cannot be reversed once they have executed. Please ensure your program has been__
__thoroughly tested before submitting orders.__

A market order executes immediately, and either buys as much bitcoin that can be
bought for a set amount of fiat currency, or sells a set amount of bitcoin for
as much fiat as possible.

Use order type @BID@ to buy bitcoin or @ASK@ to sell.

@Perm_W_Orders@ permission is required.
 -}
postMarketOrder :: BitXAuth -> MarketOrderRequest -> IO (BitXAPIResponse OrderID)
postMarketOrder auth moreq = simpleBitXPOSTAuth_ auth moreq "marketorder"
