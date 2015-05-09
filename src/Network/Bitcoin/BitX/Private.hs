{-# LANGUAGE DeriveGeneric, QuasiQuotes #-}

module Network.Bitcoin.BitX.Private
  (
  getAllOrders,
  postOrder,
  stopOrder,
  getPendingOrders,
  getOrder,
  getBalances,
  getFundingAddress,
  newFundingAddress,
  getWithdrawalRequests
  ) where

import Network.Bitcoin.BitX.Internal
import Network.Bitcoin.BitX.Types
import qualified Data.Text as Txt
import Data.Text (Text)

{- | Returns a list of the most recently placed orders.

If the second parameter is @Nothing@ then this will return orders for all markets, whereas if it is
@Just cpy@ for some @CcyPair cpy@ then the results will be specific to that market.

This list is truncated after 100 items. -}

getAllOrders :: BitXAuth -> Maybe CcyPair -> IO (Maybe (Either BitXError PrivateOrders))
getAllOrders auth pair = simpleBitXGetAuth_ auth url
    where
        url = "listorders" ++ case pair of
            Nothing  -> ""
            Just st  -> "?pair=" ++ show st

{- | Returns a list of the most recently placed orders which are still in 'PENDING' state.

If the second parameter is @Nothing@ then this will return orders for all markets, whereas if it is
@Just cpy@ for some @CcyPair cpy@ then the results will be specific to that market.

This list is truncated after 100 items. -}

getPendingOrders :: BitXAuth -> Maybe CcyPair -> IO (Maybe (Either BitXError PrivateOrders))
getPendingOrders auth pair = simpleBitXGetAuth_ auth url
    where
        url = "listorders?state=PENDING" ++ case pair of
            Nothing  -> ""
            Just st  -> "&pair=" ++ show st

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

getBalances :: BitXAuth -> IO (Maybe (Either BitXError Balances))
getBalances auth = simpleBitXGetAuth_ auth $ "balance"

{- | Returns the default receive address associated with your account and the amount received via
the address

You can specify an optional address parameter to return information for a non-default receive
address. In the response, total_received is the total confirmed Bitcoin amount received excluding
unconfirmed transactions. total_unconfirmed is the total sum of unconfirmed receive transactions. -}

getFundingAddress :: BitXAuth -> Asset -> Maybe Text -> IO (Maybe (Either BitXError FundingAddress))
getFundingAddress auth asset addr = simpleBitXGetAuth_ auth url
    where
        url = "funding_address?asset=" ++ show asset ++ case addr of
            Nothing  -> ""
            Just ad  -> "&address=" ++ (show . Txt.unpack $ ad)

{- | Create receive address

Allocates a new receive address to your account. There is a limit of 50 receive addresses per user.
-}

newFundingAddress :: BitXAuth -> Asset -> IO (Maybe (Either BitXError FundingAddress))
newFundingAddress auth asset = simpleBitXPOSTAuth_ auth asset $ "funding_address"

{- | List withdrawal requests

Returns a list of withdrawal requests. -}

getWithdrawalRequests :: BitXAuth -> IO (Maybe (Either BitXError Withdrawals))
getWithdrawalRequests auth = simpleBitXGetAuth_ auth $ "withdrawals"

