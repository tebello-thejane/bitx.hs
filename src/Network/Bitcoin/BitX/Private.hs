{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Bitcoin.BitX.Private
-- Copyright   :  No Rights Reserved
-- License     :  Public Domain
--
-- Maintainer  :  Tebello Thejane <zyxoas+hackage@gmail.com>
-- Stability   :  Experimental
-- Portability :  non-portable (GHC Extensions)
--
-- =The private BitX API.
--
-- Each one of the calls takes at least a 'BitXAuth' containing a previously-created API id and
-- secret (created by 'Network.Bitcoin.BitX.Private.Auth.authGrant', or created by visiting
-- <https://bitx.co/settings#/api_keys>), and may either return a
-- useful 'record', a 'BitXError' if BitX actually returned an error, or 'Nothing' if some exception
-- occured (or if the data returned by BitX was unparseable).
--
-- =Permissions
--
-- Each API key is granted a set of permissions when it is created. The key can only be used to call
-- the permitted API functions.
--
-- Here is a list of the possible permissions:
--
-- * @Perm_R_Balance = 1@ (View balance)
-- * @Perm_R_Transactions = 2@ (View transactions)
-- * @Perm_W_Send = 4@ (Send to any address)
-- * @Perm_R_Addresses = 8@ (View addresses)
-- * @Perm_W_Addresses = 16@ (Create addresses)
-- * @Perm_R_Orders = 32@ (View orders)
-- * @Perm_W_Orders = 64@ (Create orders)
-- * @Perm_R_Withdrawals = 128@ (View withdrawals)
-- * @Perm_W_Withdrawals = 256@ (Create withdrawals)
-- * @Perm_R_Merchant = 512@ (View merchant invoices)
-- * @Perm_W_Merchant = 1024@ (Create merchant invoices)
--
-- A set of permissions is represented as the bitwise OR of each permission in the set. For example
-- the set of permissions required to view balances and orders is @Perm_R_Balance | Perm_R_Orders =
-- 33.@
--
-----------------------------------------------------------------------------

module Network.Bitcoin.BitX.Private
  (
  getAllOrders,
  postOrder,
  stopOrder,
  getOrder,
  getBalances,
  getFundingAddress,
  newFundingAddress,
  getWithdrawalRequests,
  newWithdrawalRequest,
  getWithdrawalRequest,
  sendToAddress,
  --cancelWithdrawalRequest,
  getTransactions,
  getPendingTransactions
  ) where

import Network.Bitcoin.BitX.Internal
import Network.Bitcoin.BitX.Types
import Network.Bitcoin.BitX.Types.Internal
import qualified Data.Text as Txt
import Data.Text (Text)
import Control.Monad (liftM)

{- | Returns a list of the most recently placed orders.

If the second parameter is @Nothing@ then this will return orders for all markets, whereas if it is
@Just cpy@ for some @CcyPair cpy@ then the results will be specific to that market.

If the third parameter is @Nothing@ then this will return orders in all states, whereas if it is
@Just COMPLETE@ or @Just PENDING@ then it will return only completed or pending orders, respectively.

This list is truncated after 100 items.

@Perm_R_Orders@ permission is required.
 -}

getAllOrders :: BitXAuth -> Maybe CcyPair -> Maybe RequestStatus -> IO (Maybe (Either BitXError [PrivateOrder]))
getAllOrders auth pair status = simpleBitXGetAuth_ auth url
    where
        url = "listorders" ++ case (pair, status) of
            (Nothing, Nothing)  -> ""
            (Just pr, Nothing)  -> "?pair=" ++ show pr
            (Nothing, Just st)  -> "?state=" ++ show st
            (Just pr, Just st)  -> "?pair=" ++ show pr ++ "&state=" ++ show st

{- | Create a new order.

__Warning! Orders cannot be reversed once they have executed. Please ensure your program has been__
__thoroughly tested before submitting orders.__

@Perm_W_Orders@ permission is required.
 -}

postOrder :: BitXAuth -> OrderRequest -> IO (Maybe (Either BitXError OrderID))
postOrder auth oreq = simpleBitXPOSTAuth_ auth oreq "postorder"

{- | Request to stop an order.

@Perm_W_Orders@ permission is required.
 -}

stopOrder :: BitXAuth -> OrderID -> IO (Maybe (Either BitXError RequestSuccess))
stopOrder auth oid = simpleBitXPOSTAuth_ auth oid "stoporder"

{- | Get an order by its ID

@Perm_R_Orders@ permission is required.
 -}

getOrder :: BitXAuth -> OrderID -> IO (Maybe (Either BitXError PrivateOrderWithTrades))
getOrder auth oid = simpleBitXGetAuth_ auth $ "orders/" ++ Txt.unpack oid

{- | Return account balances

@Perm_R_Balance@ permission required. -}

getBalances :: BitXAuth -> IO (Maybe (Either BitXError [Balance]))
getBalances auth = simpleBitXGetAuth_ auth "balance"

{- | Returns the default receive address associated with your account and the amount received via
the address

You can specify an optional address parameter to return information for a non-default receive
address. In the response, total_received is the total confirmed Bitcoin amount received excluding
unconfirmed transactions. total_unconfirmed is the total sum of unconfirmed receive transactions.

@Perm_R_Addresses@ permission is required.
-}

getFundingAddress :: BitXAuth -> Asset -> Maybe String -> IO (Maybe (Either BitXError FundingAddress))
getFundingAddress auth asset addr = simpleBitXGetAuth_ auth url
    where
        url = "funding_address?asset=" ++ show asset ++ case addr of
            Nothing -> ""
            Just ad -> "&address=" ++ ad

{- | Create receive address

Allocates a new receive address to your account. There is a limit of 50 receive addresses per user.

@Perm_R_Addresses@ permission is required.
-}

newFundingAddress :: BitXAuth -> Asset -> IO (Maybe (Either BitXError FundingAddress))
newFundingAddress auth asset = simpleBitXPOSTAuth_ auth asset "funding_address"

{- | List withdrawal requests

Returns a list of withdrawal requests.

@Perm_R_Withdrawals@ permission required.-}

getWithdrawalRequests :: BitXAuth -> IO (Maybe (Either BitXError [WithdrawalRequest]))
getWithdrawalRequests auth = simpleBitXGetAuth_ auth "withdrawals/"

{- | Request a withdrawal

Creates a new withdrawal request.

@Perm_W_Withdrawals@ permission required.-}

newWithdrawalRequest :: BitXAuth -> NewWithdrawal -> IO (Maybe (Either BitXError WithdrawalRequest))
newWithdrawalRequest auth nwithd = simpleBitXPOSTAuth_ auth nwithd "withdrawals"

{- | Get the status of a withdrawal request

Returns the status of a particular withdrawal request.

@Perm_R_Withdrawals@ permission required.-}

getWithdrawalRequest :: BitXAuth -> Text -- ^ The withdrawal ID
    -> IO (Maybe (Either BitXError WithdrawalRequest))
getWithdrawalRequest auth wthid = simpleBitXGetAuth_ auth $ "withdrawals/" ++ Txt.unpack wthid

{- | Cancel a withdrawal request

This can only be done if the request is still in state PENDING.

@Perm_W_Withdrawals@ permission required.-}

--cancelWithdrawalRequest :: BitXAuth -> String -> IO (Maybe (Either BitXError WithdrawalRequest))
--cancelWithdrawalRequest auth wthid = simpleBitXMETHAuth_ auth "DELETE" $ "withdrawals/" ++ wthid

{- | Send Bitcoin from your account to a Bitcoin address or email address.

If the email address is not associated with an existing BitX account, an invitation to create an account
and claim the funds will be sent.

__Warning! Bitcoin transactions are irreversible. Please ensure your program has been thoroughly__
__tested before using this call.__


@Perm_W_Send@ permission required. Note that when creating an API key on the BitX site, selecting
"Full access" is not sufficient to add the @Perm_W_Send@ permission. Instead, the permission needs
to be enabled explicitely by selecting "Custom."-}

sendToAddress :: BitXAuth -> BitcoinSendRequest -> IO (Maybe (Either BitXError RequestSuccess))
sendToAddress auth sreq = simpleBitXPOSTAuth_ auth sreq "send"

{- | Return a list of transaction entries from an account.

Transaction entry rows are numbered sequentially starting from 1, where 1 is the oldest entry. The
range of rows to return are specified with the min_row (inclusive) and max_row (exclusive)
parameters. At most 1000 rows can be requested per call.

If min_row or max_row is nonpositive, the range wraps around the most recent row. For example, to
fetch the 100 most recent rows, use min_row=-100 and max_row=0.

@Perm_R_Transactions@ permission required.
-}

getTransactions
    :: BitXAuth
    -> AccountID
    -> Int -- ^ First row returned, inclusive
    -> Int -- ^ Last row returned, exclusive
    -> IO (Maybe (Either BitXError [Transaction]))
getTransactions auth accid minr maxr = simpleBitXGetAuth_ auth $
    "accounts/" ++ Txt.unpack accid ++ "/transactions?min_row=" ++ show minr ++ "&max_row=" ++ show maxr

{- | Pending transactions

Return a list of all pending transactions related to the account.

Unlike account entries, pending transactions are not numbered, and may be reordered, deleted or
updated at any time.

@Perm_R_Transactions@ permission required.
-}

getPendingTransactions :: BitXAuth -> AccountID -> IO (Maybe (Either BitXError [Transaction]))
getPendingTransactions auth accid = liftM imebPendingTransactionsToimebTransactions $ simpleBitXGetAuth_ auth $
    "accounts/" ++ Txt.unpack accid ++ "/pending"
    where
        --imebPendingTransactionsToimebTransactions :: Maybe (Either BitXError PendingTransactions__) -> Maybe (Either BitXError [Transaction])
        imebPendingTransactionsToimebTransactions (Just (Right pt)) = Just . Right $ pendingTransactionsToTransactions pt
        imebPendingTransactionsToimebTransactions (Just (Left er)) = Just . Left $ er
        imebPendingTransactionsToimebTransactions Nothing = Nothing

