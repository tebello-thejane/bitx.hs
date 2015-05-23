{-# LANGUAGE DeriveGeneric, DefaultSignatures, QuasiQuotes, OverloadedStrings, DataKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Bitcoin.BitX.Types
-- Copyright   :  No Rights Reserved
-- License     :  Public Domain
--
-- Maintainer  :  Tebello Thejane <zyxoas+hackage@gmail.com>
-- Stability   :  Experimental
-- Portability :  non-portable (GHC Extensions)
--
-- The types used for the various BitX API calls.
--
-- Note that these are all `record` types, as provided by Nikita Volkov's
-- "Record" library. The main motivation for using the @record@ library was
-- to avoid using record field prefixes and other awkward hacks to get around
-- the fact that Haskell does not yet have a real records' system.
--
-- For example, the declaration of `BitXError` is
--
-- @
-- type BitXAuth =
--     ['record'|
--         {id :: 'Text',
--          secret :: 'Text'} |]
-- @
--
-- To declare a BitXAuth, one might use
--
-- @
-- myAuth :: BitXAuth
-- myAuth =
--     [record|
--         {id = "46793",
--          secret = "387ffBd56eEAA7C59"} |]
-- @
--
-- and to read the fields you would use
--
-- @
-- theID = 'view' ['lens'| id |] myAuth
-- @
--
-- Note that all uses of Volkov's `record`s requires importing "Record" and
-- enabling the 'DataKinds' and 'QuasiQuotes' extensions.
--
-- See <http://nikita-volkov.github.io/record/>
--
-----------------------------------------------------------------------------

module Network.Bitcoin.BitX.Types
  (
    Ticker,
    CcyPair(..),
    Orderbook,
    Order,
    Bid,
    Ask,
    Trade,
    BitXAuth,
    PrivateOrder,
    OrderID,
    OrderType(..),
    RequestStatus(..),
    OrderRequest,
    RequestSuccess,
    BitXError,
    PrivateOrderWithTrades,
    AccountID,
    Asset(..),
    Balance,
    FundingAddress,
    WithdrawalRequest,
    NewWithdrawal,
    WithdrawalType(..),
    BitcoinSendRequest,
    QuoteRequest,
    OrderQuote,
    QuoteType(..),
    BitXClientAuth,
    Transaction
    --, Transactions
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Text (Text)
import Data.Time.Clock
import Record
import GHC.Generics (Generic)
import Data.Decimal

-- | A possible error which the BitX API might return,
-- instead of returning the requested data. Note that as yet there is no
-- exhaustive list of error codes available, so comparisons will have to be
-- done via Text comparisons (as opposed to typed pattern matching). Sorry...
--
-- @
--type BitXError =
--    [record|
--        {error :: 'Text',
--         errorCode :: 'Text'} |]
-- @

type BitXError =
    [record|
        {error :: Text,
         errorCode :: Text} |]

-- | The state of a single market, identified by the currency pair.
-- As usual, the ask\/sell price is the price of the last filled ask order, and the bid\/buy price is
-- the price of the last filled bid order. Necessarily @bid <= ask.@
--
-- @
--type Ticker =
--    [record|
--        {ask :: 'Decimal',
--         timestamp :: 'UTCTime',
--         bid :: 'Decimal',
--         rolling24HourVolume :: 'Decimal',
--         lastTrade :: 'Decimal',
--         pair :: 'CcyPair'} |]
-- @

type Ticker =
    [record|
        {ask :: Decimal,
         timestamp :: UTCTime,
         bid :: Decimal,
         rolling24HourVolume :: Decimal,
         lastTrade :: Decimal,
         pair :: CcyPair} |]

-- | A currency pair
data CcyPair =
    XBTZAR -- ^ Bitcoin vs. ZAR
    | XBTNAD -- ^  Bitcoin vs. Namibian Dollar
    | ZARXBT -- ^ ZAR vs. Namibian Dollar
    | NADXBT -- ^ Namibian Dollar vs. Bitcoin
    | XBTKES -- ^ Bitcoin vs. Kenyan Shilling
    | KESXBT -- ^ Kenyan Shilling vs Bitcoin
    | XBTMYR -- ^ Bitcoin vs. Malaysian Ringgit
    | MYRXBT -- ^ Malaysian Ringgit vs. Bitcoin
  deriving (Show, Read, Generic, Eq)

-- | A trade-able asset. Essentially, a currency.
data Asset =
    ZAR -- ^ South African Rand
    | NAD -- ^ Namibian Dollar
    | XBT -- ^ Bitcoin
    | KES -- ^ Kenyan Shilling
    | MYR -- ^ Malaysian Ringgit
  deriving (Show, Read, Generic, Eq)

-- | The current state of the publically accessible orderbook.
-- Bid orders are requests to buy, ask orders are requests to sell.
--
-- @
--type Orderbook =
--    [record|
--        {timestamp :: 'UTCTime',
--         bids :: ['Bid'],
--         asks :: ['Ask']} |]
-- @

type Orderbook =
    [record|
        {timestamp :: UTCTime,
         bids :: [Bid],
         asks :: [Ask]} |]

-- | A single placed order in the orderbook
--
-- @
--type Order =
--    [record|
--        {volume :: 'Decimal',
--         price :: 'Decimal'} |]
-- @

type Order =
    [record|
        {volume :: Decimal,
         price :: Decimal} |]

-- | Convenient type alias for a bid order
type Bid = Order

-- | Convenient type alias for an ask order
type Ask = Order

type Trade =
    [record|
        {volume :: Decimal,
         timestamp :: UTCTime,
         price :: Decimal} |]

-- | An auth type used by all private API calls, after authorisation.
--
-- @
--type BitXAuth =
--    [record|
--        {id :: 'Text',
--         secret :: 'Text'} |]
-- @
type BitXAuth =
    [record|
        {id :: Text,
         secret :: Text} |]

type BitXClientAuth = BitXAuth

-- | A recently placed (private) order, containing a lot more information than is available on the
-- public order book.
--
-- @
--type PrivateOrder =
--    [record|
--        {base :: 'Decimal',
--         counter :: 'Decimal',
--         creationTimestamp :: 'UTCTime',
--         expirationTimestamp :: 'UTCTime',
--         feeBase :: 'Decimal',
--         feeCounter :: 'Decimal',
--         limitPrice :: 'Decimal',
--         limitVolume :: 'Decimal',
--         id :: 'OrderID',
--         pair :: 'CcyPair',
--         state :: 'RequestStatus',
--         type :: 'OrderType' } |]
-- @
type PrivateOrder =
    [record|
        {base :: Decimal,
         counter :: Decimal,
         creationTimestamp :: UTCTime,
         expirationTimestamp :: UTCTime,
         feeBase :: Decimal,
         feeCounter :: Decimal,
         limitPrice :: Decimal,
         limitVolume :: Decimal,
         id :: OrderID,
         pair :: CcyPair,
         state :: RequestStatus,
         type :: OrderType } |]

-- | A recently placed (private) order, containing a lot more information than is available on the
-- public order book, together with details of any trades which have (partially) filled it.
--
-- @
--type PrivateOrderWithTrades =
--    [record|
--        {base :: 'Decimal',
--         counter :: 'Decimal',
--         creationTimestamp :: 'UTCTime',
--         expirationTimestamp :: 'UTCTime',
--         feeBase :: 'Decimal',
--         feeCounter :: 'Decimal',
--         limitPrice :: 'Decimal',
--         limitVolume :: 'Decimal',
--         id :: 'OrderID',
--         pair :: 'CcyPair',
--         state :: 'RequestStatus',
--         type :: 'OrderType',
--         trades :: ['Trade'] } |]
-- @
type PrivateOrderWithTrades =
    [record|
        {base :: Decimal,
         counter :: Decimal,
         creationTimestamp :: UTCTime,
         expirationTimestamp :: UTCTime,
         feeBase :: Decimal,
         feeCounter :: Decimal,
         limitPrice :: Decimal,
         limitVolume :: Decimal,
         id :: OrderID,
         pair :: CcyPair,
         state :: RequestStatus,
         type :: OrderType,
         trades :: [Trade] } |]

-- | A transaction on a private user account.
--
-- @
--type Transaction =
--    [record|
--        {rowIndex :: 'Int',
--         timestamp :: 'UTCTime',
--         balance :: 'Decimal',
--         available :: 'Decimal',
--         balanceDelta :: 'Decimal',
--         availableDelta :: 'Decimal',
--         currency :: 'Asset',
--         description :: 'Text'}|]
-- @
type Transaction =
    [record|
        {rowIndex :: Int,
         timestamp :: UTCTime,
         balance :: Decimal,
         available :: Decimal,
         balanceDelta :: Decimal,
         availableDelta :: Decimal,
         currency :: Asset,
         description :: Text}|]

type OrderID = Text

-- | The type of a placed order.
data OrderType =
    ASK -- ^ A request to sell
    | BID -- ^ A request to buy
    deriving (Show, Read, Generic, Eq)

-- | The state of a (private) placed request -- either an order or a withdrawal request.
data RequestStatus =
    PENDING -- ^ Not yet completed. An order will stay in 'PENDING' state even as it is partially
    -- filled, and will move to 'COMPLETE' once it has been completely filled.
    | COMPLETE -- ^ Completed.
    | CANCELLED -- ^ Cancelled. Note that an order cannot be in  'CANCELLED' state, since cancelling
    -- an order removes it from the orderbook.
    deriving (Show, Read, Generic, Eq)

-- | A request to place an order.
--
-- @
--type OrderRequest =
--    [record|
--        {pair :: 'CcyPair',
--         type :: 'OrderType',
--         volume :: 'Decimal',
--         price :: 'Decimal' } |]
-- @
type OrderRequest =
    [record|
        {pair :: CcyPair,
         type :: OrderType,
         volume :: Decimal,
         price :: Decimal } |]

type AccountID = Text

-- | The current balance of a private account.
--
-- @
--type Balance =
--    [record|
--        {id :: 'AccountID',
--         asset :: 'Asset',
--         balance :: 'Decimal',
--         reserved :: 'Decimal',
--         unconfirmed :: 'Decimal' } |]
-- @
type Balance =
    [record|
        {id :: AccountID,
         asset :: Asset,
         balance :: Decimal,
         reserved :: Decimal,
         unconfirmed :: Decimal } |]

-- | A registered address for an acocunt.
--
-- @
--type FundingAddress =
--    [record|
--        {asset :: 'Asset',
--         address :: 'Text',
--         totalReceived :: 'Decimal',
--         totalUnconfirmed :: 'Decimal'} |]
-- @
type FundingAddress =
    [record|
        {asset :: Asset,
         address :: Text,
         totalReceived :: Decimal,
         totalUnconfirmed :: Decimal} |]

-- | The state of a request to withdraw from an account.
--
-- @
--type WithdrawalRequest =
--    [record|
--        {status :: 'RequestStatus',
--         id :: 'Text' } |]
-- @
type WithdrawalRequest =
    [record|
        {status :: RequestStatus,
         id :: Text } |]

-- | A request to withdraw from an account.
--
-- @
--type NewWithdrawal =
--    [record|
--        {type :: 'WithdrawalType',
--         amount :: 'Decimal' } |]
-- @
type NewWithdrawal =
    [record|
        {type :: WithdrawalType,
         amount :: Decimal } |]

-- | A request to send bitcoin to a bitcoin address or email address.
--
-- @
--type BitcoinSendRequest =
--    [record|
--        {amount :: 'Decimal',
--         currency :: 'Asset',
--         address :: 'Text',
--         description :: 'Maybe' 'Text',
--         message :: 'Maybe' 'Text',
--         pin :: 'Text'} |]
-- @
type BitcoinSendRequest =
    [record|
        {amount :: Decimal,
         currency :: Asset,
         address :: Text,
         description :: Maybe Text,
         message :: Maybe Text,
         pin :: Text} |]

-- | A request to lock in a quote.
--
-- @
--type QuoteRequest =
--    [record|
--        {type :: 'QuoteType',
--         pair :: 'CcyPair',
--         baseAmount :: 'Decimal'} |]
-- @
type QuoteRequest =
    [record|
        {type :: QuoteType,
         pair :: CcyPair,
         baseAmount :: Decimal} |]

-- | A temporarily locked in quote.
--
-- @
--type OrderQuote =
--    [record|
--        {id :: 'Text',
--         type :: 'QuoteType',
--         pair :: 'CcyPair',
--         baseAmount :: 'Decimal',
--         counterAmount :: 'Decimal',
--         createdAt :: 'UTCTime',
--         expiresAt :: 'UTCTime',
--         discarded :: 'Bool',
--         exercised :: 'Bool'} |]
-- @
type OrderQuote =
    [record|
        {id :: Text,
         type :: QuoteType,
         pair :: CcyPair,
         baseAmount :: Decimal,
         counterAmount :: Decimal,
         createdAt :: UTCTime,
         expiresAt :: UTCTime,
         discarded :: Bool,
         exercised :: Bool} |]

-- | The type of a withdrawal request.
data WithdrawalType =
    ZAR_EFT -- ^ ZAR by Electronic Funds Transfer
    | NAD_EFT -- ^ Namibian Dollar by EFT
    | KES_MPESA -- ^ Kenyan Shilling by Vodafone MPESA
    | MYR_IBG -- ^ Malaysian Ringgit by Interbank GIRO (?)
    | IDR_LLG -- ^ Indonesian Rupiah by Lalu Lintas Giro (??)
    deriving (Show, Read, Generic, Eq)

data QuoteType = BUY | SELL deriving (Show, Read, Generic, Eq)

type RequestSuccess = Bool

instance ToJSON CcyPair
instance FromJSON CcyPair

instance ToJSON Asset
instance FromJSON Asset

instance ToJSON OrderType
instance FromJSON OrderType

--instance ToJSON RequestStatus
--instance FromJSON RequestStatus

instance ToJSON WithdrawalType
instance FromJSON WithdrawalType

instance ToJSON QuoteType
instance FromJSON QuoteType
