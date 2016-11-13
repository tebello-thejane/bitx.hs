{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Bitcoin.BitX.Private
-- Copyright   :  2016 Tebello Thejane
-- License     :  BSD3
--
-- Maintainer  :  Tebello Thejane <zyxoas+hackage@gmail.com>
-- Stability   :  Experimental
-- Portability :  non-portable (GHC Extensions)
--
-- =The private BitX API.
--
-- Each one of the calls takes at least a 'BitXAuth' containing a previously-created API id and
-- secret (created by visiting
-- <https://bitx.co/settings#/api_keys>), and will return a 'BitXAPIResponse'..
--
-- It would probably be best to create the 'BitXAuth' record using the constructor 'mkBitXAuth' and
-- lens setters:
--
-- @
-- let auth = mkBitXAuth
--              '&' BitX.id '.~' "dude"
--              & BitX.secret .~ "mySecret"
-- let resp = BitX.getBalances auth
-- ...
-- @
--
-- With the OverloadedStrings extension enabled, one may also simply write
-- @
-- let auth = "dude:mySecret" :: BitXAuth
-- @
-- with a colon separating the id from the secret.
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
-- * @Perm_W_ClientDebit = 8192@ (Debit accounts)
-- * @Perm_W_ClientCredit = 16384@ (Credit accounts)
-- * @Perm_R_Beneficiaries = 32768@ (View beneficiaries)
-- * @Perm_W_Beneficiaries = 65536@ (Create and delete beneficiaries)
--
-- A set of permissions is represented as the bitwise OR of each permission in the set. For example
-- the set of permissions required to view balances and orders is @Perm_R_Balance | Perm_R_Orders =
-- 33.@
--
-----------------------------------------------------------------------------

module Network.Bitcoin.BitX.Private
  (
  newAccount,
  getBalances,
  getFundingAddress,
  newFundingAddress,
  sendToAddress,
  getTransactions,
  getPendingTransactions,

  module Network.Bitcoin.BitX.Private.Order,
  module Network.Bitcoin.BitX.Private.Quote,
  --module Network.Bitcoin.BitX.Private.Auth
  module Network.Bitcoin.BitX.Private.Withdrawal
  ) where

import Network.Bitcoin.BitX.Internal
import Network.Bitcoin.BitX.Types
import Network.Bitcoin.BitX.Types.Internal
import qualified Data.Text as Txt
import Network.Bitcoin.BitX.Response

import Network.Bitcoin.BitX.Private.Order
--import Network.Bitcoin.BitX.Private.Auth
import Network.Bitcoin.BitX.Private.Quote
import Network.Bitcoin.BitX.Private.Withdrawal
import Data.Monoid ((<>))

{-# ANN module ("HLint: ignore Use import/export shortcut" :: String) #-}

{- | Create an additional account for the specified currency

Note that the 'id' field of the second parameter can be left blank. The call will return an `Account`
object resembling the parameter, but with the 'id' field filled in with the newly created account's
id.

You must be verified to trade the currency in question in order to be able to create an account.

@Perm_W_Addresses@ permission required. -}

newAccount :: BitXAuth -> Account -> IO (BitXAPIResponse Account)
newAccount auth acc = simpleBitXPOSTAuth_ auth acc "accounts"

{- | Return account balances

@Perm_R_Balance@ permission required. -}

getBalances :: BitXAuth -> IO (BitXAPIResponse [Balance])
getBalances auth = simpleBitXGetAuth_ auth "balance"

{- | Returns the default receive address associated with your account and the amount received via
the address

You can specify an optional address parameter to return information for a non-default receive
address. In the response, total_received is the total confirmed Bitcoin amount received excluding
unconfirmed transactions. total_unconfirmed is the total sum of unconfirmed receive transactions.

@Perm_R_Addresses@ permission is required.
-}

getFundingAddress :: BitXAuth -> Asset -> Maybe Txt.Text -> IO (BitXAPIResponse FundingAddress)
getFundingAddress auth fasset addr = simpleBitXGetAuth_ auth url
    where
      url = "funding_address?asset=" <> Txt.pack (show fasset) <> case addr of
            Nothing -> ""
            Just ad -> "&address=" <> ad

{- | Create receive address

Allocates a new receive address to your account. Address creation is rate limited to 1 per hour,
allowing for bursts of up to 10 consecutive calls.

@Perm_R_Addresses@ permission is required.
-}

newFundingAddress :: BitXAuth -> Asset -> IO (BitXAPIResponse FundingAddress)
newFundingAddress auth fasset = simpleBitXPOSTAuth_ auth fasset "funding_address"

{- | Send Bitcoin from your account to a Bitcoin address or email address.

If the email address is not associated with an existing BitX account, an invitation to create an account
and claim the funds will be sent.

__Warning! Bitcoin transactions are irreversible. Please ensure your program has been thoroughly__
__tested before using this call.__


@Perm_W_Send@ permission required. Note that when creating an API key on the BitX site, selecting
"Full access" is not sufficient to add the @Perm_W_Send@ permission. Instead, the permission needs
to be enabled explicitely by selecting "Custom."-}

sendToAddress :: BitXAuth -> BitcoinSendRequest -> IO (BitXAPIResponse RequestSuccess)
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
    -> IO (BitXAPIResponse [Transaction])
getTransactions auth accid minr maxr = simpleBitXGetAuth_ auth $
    "accounts/" <> accid <> "/transactions?min_row=" <> Txt.pack (show minr) <> "&max_row=" <> Txt.pack (show maxr)

{- | Pending transactions

Return a list of all pending transactions related to the account.

Unlike account entries, pending transactions are not numbered, and may be reordered, deleted or
updated at any time.

@Perm_R_Transactions@ permission required.
-}

getPendingTransactions :: BitXAuth -> AccountID -> IO (BitXAPIResponse [Transaction])
getPendingTransactions auth accid = fmap imebPendingTransactionsToimebTransactions $ simpleBitXGetAuth_ auth $
    "accounts/" <> accid <> "/pending"
    where
        imebPendingTransactionsToimebTransactions (ValidResponse v)         = ValidResponse $ pendingTransactionsToTransactions v
        imebPendingTransactionsToimebTransactions (ExceptionResponse x)     = ExceptionResponse x
        imebPendingTransactionsToimebTransactions (ErrorResponse e)         = ErrorResponse e
        imebPendingTransactionsToimebTransactions (UnparseableResponse a u) = UnparseableResponse a u
