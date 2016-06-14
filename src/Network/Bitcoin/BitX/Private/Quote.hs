{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Bitcoin.BitX.Private.Quote
-- Copyright   :  2016 Tebello Thejane
-- License     :  BSD3
--
-- Maintainer  :  Tebello Thejane <zyxoas+hackage@gmail.com>
-- Stability   :  Experimental
-- Portability :  non-portable (GHC Extensions)
--
-- The API for dealing with quotes.
--
-- Quotes allow you to lock in an exchange rate for a short time with the
-- option of either accepting or rejecting the quote.
--
-- Quotes can be useful for various customer-facing applications where
-- price fluctuations would be confusing.
--
-- The API is used as follows: First create a quote for the transaction that
-- you want to perform. If you decide to accept the quote before it expires,
-- you will exercise the quote. If you decide not to accept it, you will
-- discard the quote. You can also retrieve the status of a quote at any time.
--
-----------------------------------------------------------------------------

module Network.Bitcoin.BitX.Private.Quote
    (
    newQuote,
    getQuote,
    exerciseQuote,
    discardQuote
    ) where

import Network.Bitcoin.BitX.Internal
import Network.Bitcoin.BitX.Types
import Data.Text (Text)
import qualified Data.Text as Txt
import Network.Bitcoin.BitX.Response
import Data.Monoid ((<>))

{- | Create a quote

Creates a new quote to buy or sell a particular amount.

You can specify either the exact amount that you want to pay or the exact amount that you want to
receive.

For example, to buy exactly 0.1 Bitcoin using ZAR, you would create a quote to BUY 0.1 XBTZAR.
The returned quote includes the appropriate ZAR amount. To buy Bitcoin using exactly ZAR 100,
you would create a quote to SELL 100 ZARXBT. The returned quote specifies the Bitcoin as the
counter amount that will be returned.

An error is returned if your account is not verified for the currency pair, or if your account
would have insufficient balance to ever exercise the quote.

The currency pair can also be flipped if you want to buy or sell the counter currency (e.g. ZARXBT).

@Perm_W_Orders@ permission required.
-}

newQuote :: BitXAuth -> QuoteRequest -> IO (BitXAPIResponse OrderQuote)
newQuote auth qreq = simpleBitXPOSTAuth_ auth qreq "quotes"

{- | Get a quote

Get the latest status of a quote, retrieved by ID.

@Perm_R_Orders@ permission required.
-}

getQuote :: BitXAuth -> Text -> IO (BitXAPIResponse OrderQuote)
getQuote auth qid = simpleBitXGetAuth_ auth $ "quotes/" <> qid

{- | Exercise a quote

Exercise a quote to perform the trade. If there is sufficient balance available in your account,
it will be debited and the counter amount credited.

An error is returned if the quote has expired or if you have insufficient available balance.

@Perm_W_Orders@ permission required.
-}

exerciseQuote :: BitXAuth -> Text -> IO (BitXAPIResponse OrderQuote)
exerciseQuote auth qid = simpleBitXMETHAuth_ auth "PUT" $ "quotes/" <> qid

{- | Discard a quote

Discard a quote. Once a quote has been discarded, it cannot be exercised even if it has not expired
yet.

@Perm_W_Orders@ permission required.
-}

discardQuote :: BitXAuth -> Text -> IO (BitXAPIResponse OrderQuote)
discardQuote auth qid = simpleBitXMETHAuth_ auth "DELETE" $ "quotes/" <> qid
