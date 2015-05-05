{-# LANGUAGE DeriveGeneric, DefaultSignatures, QuasiQuotes, OverloadedStrings #-}

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
    OrderStatus(..),
    OrderRequest,
    StopOrderSuccess(..),
    PublicTrades,
    BitXError,
    Tickers
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Text (Text)
import Data.Time.Clock
import Record
import GHC.Generics (Generic)
import Data.Decimal

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
         state :: OrderStatus,
         orderType :: OrderType } |]

type OrderID = Text

data OrderType = ASK | BID deriving (Show, Read, Generic)

data OrderStatus = PENDING | COMPLETE deriving (Show, Read, Generic)

type OrderRequest =
    [record|
        {pair :: CcyPair,
         requestType :: OrderType,
         volume :: Decimal,
         price :: Decimal } |]

data StopOrderSuccess = StopOrderSuccess deriving (Show, Read, Generic)

instance ToJSON CcyPair
instance FromJSON CcyPair

instance ToJSON OrderType
instance FromJSON OrderType

instance ToJSON OrderStatus
instance FromJSON OrderStatus

instance ToJSON StopOrderSuccess
instance FromJSON StopOrderSuccess
