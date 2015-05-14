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
    PublicTrades,
    BitXError,
    Tickers,
    PrivateOrders,
    PrivateOrderWithTrades,
    AccountID,
    Asset(..),
    Balance,
    Balances,
    FundingAddress,
    WithdrawalRequests,
    WithdrawalRequest,
    NewWithdrawal,
    WithdrawalType(..),
    BitcoinSendRequest,
    QuoteRequest,
    OrderQuote,
    QuoteType(..),
    BitXClientAuth
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Text (Text)
import Data.Time.Clock
import Record
import GHC.Generics (Generic)
import Data.Decimal

-- | A record representing a possible error which the BitX API might return,
-- instead of returning the requested data. Note that as yet there is no
-- exhaustive list of error codes available, so comparisons will have to be
-- done via Text comparisons (as opposed to typed pattern matching). Sorry...
type BitXError =
    [record|
        {error :: Text,
         errorCode :: Text} |]

type Ticker =
    [record|
        {ask :: Decimal,
         timestamp :: UTCTime,
         bid :: Decimal,
         rolling24HourVolume :: Decimal,
         lastTrade :: Decimal,
         pair :: CcyPair} |]

type Tickers =
    [record|
        {tickers :: [Ticker]} |]

data CcyPair = XBTZAR | XBTNAD | ZARXBT | NADXBT | XBTKES | KESXBT | XBTMYR | MYRXBT
  deriving (Show, Read, Generic)

data Asset = ZAR | NAD | XBT | KES | MYR
  deriving (Show, Read, Generic)

type Orderbook =
    [record|
        {timestamp :: UTCTime,
         bids :: [Bid],
         asks :: [Ask]} |]

type Order =
    [record|
        {volume :: Decimal,
         price :: Decimal} |]

type Bid = Order
type Ask = Order

type Trade =
    [record|
        {volume :: Decimal,
         timestamp :: UTCTime,
         price :: Decimal} |]

type PublicTrades =
    [record|
        {trades :: [Trade],
         currency :: Text} |]

type BitXAuth =
    [record|
        {id :: Text,
         secret :: Text} |]

type BitXClientAuth = BitXAuth

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
         orderID :: OrderID,
         pair :: CcyPair,
         state :: RequestStatus,
         orderType :: OrderType } |]

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
         orderID :: OrderID,
         pair :: CcyPair,
         state :: RequestStatus,
         orderType :: OrderType,
         trades :: [Trade] } |]

type PrivateOrders =
    [record|
        {orders :: [PrivateOrder]} |]

type OrderID = Text

data OrderType = ASK | BID deriving (Show, Read, Generic)

data RequestStatus = PENDING | COMPLETE | CANCELLED deriving (Show, Read, Generic)

type OrderRequest =
    [record|
        {pair :: CcyPair,
         requestType :: OrderType,
         volume :: Decimal,
         price :: Decimal } |]

type AccountID = Text

type Balance =
    [record|
        {accountID :: AccountID,
         asset :: Asset,
         balance :: Decimal,
         reserved :: Decimal,
         unconfirmed :: Decimal } |]

type Balances =
    [record|
        {balances :: [Balance] } |]

type FundingAddress =
    [record|
        {asset :: Asset,
         address :: Text,
         totalReceived :: Decimal,
         totalUnconfirmed :: Decimal} |]

type WithdrawalRequests =
    [record|
        {withdrawalRequests :: [WithdrawalRequest]} |]

type WithdrawalRequest =
    [record|
        {status :: RequestStatus,
         id :: Text } |]

type NewWithdrawal =
    [record|
        {withdrawalType :: WithdrawalType,
         amount :: Decimal } |]

type BitcoinSendRequest =
    [record|
        {amount :: Decimal,
         currency :: Asset,
         address :: Text,
         description :: Maybe Text,
         message :: Maybe Text,
         pin :: Text} |]

type QuoteRequest =
    [record|
        {type :: QuoteType,
         pair :: CcyPair,
         baseAmount :: Decimal} |]

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

data WithdrawalType = ZAR_EFT | NAD_EFT | KES_MPESA | MYR_IBG | IDR_LLG deriving (Show, Read, Generic)

data QuoteType = BUY | SELL deriving (Show, Read, Generic)

type RequestSuccess = Bool

instance ToJSON CcyPair
instance FromJSON CcyPair

instance ToJSON Asset
instance FromJSON Asset

instance ToJSON OrderType
instance FromJSON OrderType

instance ToJSON RequestStatus
instance FromJSON RequestStatus

instance ToJSON WithdrawalType
instance FromJSON WithdrawalType

instance ToJSON QuoteType
instance FromJSON QuoteType
