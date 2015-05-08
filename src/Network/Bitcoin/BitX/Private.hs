{-# LANGUAGE DeriveGeneric, QuasiQuotes #-}

module Network.Bitcoin.BitX.Private
  (
  getAllOrders,
  postOrder,
  stopOrder,
  getPendingOrders,
  getOrder
  ) where

import Network.Bitcoin.BitX.Internal
import Network.Bitcoin.BitX.Types
import Record (lens)
import Record.Lens (view)
import qualified Data.Text as Txt

{- | Returns a list of the most recently placed orders.

If the second parameter is @Nothing@ then this will return orders for all markets, whereas if it is
@Just cpy@ for some @CcyPair cpy@ then the results will be specific to that market.

This list is truncated after 100 items. -}

getAllOrders :: BitXAuth -> Maybe CcyPair -> IO (Maybe (Either BitXError PrivateOrders))
getAllOrders auth pair = simpleBitXGetAuth_ auth url
    where
        url = case pair of
            Nothing  -> "listorders"
            Just st  -> "listorders?pair=" ++ show st

{- | Returns a list of the most recently placed orders which are still in 'PENDING' state.

If the second parameter is @Nothing@ then this will return orders for all markets, whereas if it is
@Just cpy@ for some @CcyPair cpy@ then the results will be specific to that market.

This list is truncated after 100 items. -}

getPendingOrders :: BitXAuth -> Maybe CcyPair -> IO (Maybe (Either BitXError PrivateOrders))
getPendingOrders auth pair = simpleBitXGetAuth_ auth url
    where
        url = case pair of
            Nothing  -> "listorders?state=PENDING"
            Just st  -> "listorders?state=PENDING&pair=" ++ show st

{- | Create a new order.

__Warning! Orders cannot be reversed once they have executed. Please ensure your program has been
thoroughly tested before submitting orders.__

-}

postOrder :: BitXAuth -> OrderRequest -> IO (Maybe (Either BitXError OrderID))
postOrder auth oreq = simpleBitXPOSTAuth_ auth oreq "postorder"

{- | Request to stop an order. -}

stopOrder :: BitXAuth -> OrderID -> IO (Maybe (Either BitXError StopOrderSuccess))
stopOrder auth oid = simpleBitXPOSTAuth_ auth oid "stoporder"

{- | Get an order by its ID -}

getOrder :: BitXAuth -> OrderID -> IO (Maybe (Either BitXError PrivateOrderWithTrades))
getOrder auth oid = simpleBitXGetAuth_ auth $ "orders/" ++ Txt.unpack oid

{- | Return account balances -}

getBalances :: BitXAuth -> OrderID -> IO (Maybe (Either BitXError Balances))
getBalances auth oid = simpleBitXGetAuth_ auth $ "balance"
