{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.BitX.Private.Quote
    (
    newQuote,
    getQuote,
    exerciseQuote,
    discardQuote
    ) where

import Network.Bitcoin.BitX.Internal
import Network.Bitcoin.BitX.Types
import qualified Data.Text as Txt
import Data.Text (Text)

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
-}

newQuote :: BitXAuth -> QuoteRequest -> IO (Maybe (Either BitXError OrderQuote))
newQuote auth qreq = simpleBitXPOSTAuth_ auth qreq "quotes"

{- | Get a quote
Get the latest status of a quote, retrieved by ID. -}

getQuote :: BitXAuth -> Text -> IO (Maybe (Either BitXError OrderQuote))
getQuote auth qid = simpleBitXGetAuth_ auth $ "quotes/" ++ Txt.unpack qid

{- | Exercise a quote
Exercise a quote to perform the trade. If there is sufficient balance available in your account,
it will be debited and the counter amount credited.

An error is returned if the quote has expired or if you have insufficient available balance. -}

exerciseQuote :: BitXAuth -> Text -> IO (Maybe (Either BitXError OrderQuote))
exerciseQuote auth qid = simpleBitXMETHAuth_ auth "PUT" $ "quotes/" ++ Txt.unpack qid

{- | Discard a quote
Discard a quote. Once a quote has been discarded, it cannot be exercised even if it has not expired yet.
-}

discardQuote :: BitXAuth -> Text -> IO (Maybe (Either BitXError OrderQuote))
discardQuote auth qid = simpleBitXMETHAuth_ auth "DELETE" $ "quotes/" ++ Txt.unpack qid

