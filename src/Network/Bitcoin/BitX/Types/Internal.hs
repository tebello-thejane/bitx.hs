{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, MultiParamTypeClasses,
    FunctionalDependencies, FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}

module Network.Bitcoin.BitX.Types.Internal
    (
    BitXRecordConvert(..),
    Ticker_(..),
    tickerConverter_,
    BitXError_(..),
    bitXErrorConverter_,
    Tickers_(..),
    tickersConverter_,
    timestampMsToUTCTime_,
    OrderRequest_(..)
    )
where

import Network.Bitcoin.BitX.Types
import Data.Aeson (ToJSON(..), FromJSON(..), parseJSON, (.:), Value(..), object, (.=))
import qualified Data.Aeson.TH as AesTH
import qualified Data.Text as Txt
import Data.Text (Text)
import Control.Applicative ((<$>), (<*>))
import Data.Time.Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Control.Monad (liftM)
import Record
import Record.Lens
import Data.Monoid (mempty)
import Network.Bitcoin.BitX.Internal
import Data.Decimal

timestampMsToUTCTime_ :: Text -> UTCTime
timestampMsToUTCTime_ xx =
     posixSecondsToUTCTime . fromRational . toRational $ ((read . reverse . div100Rev
        . reverse . Txt.unpack $ xx) :: Decimal)
     where
         div100Rev st = (take 3 st) ++ "." ++ (drop 3 st)

timestampParse_ :: Integer -> UTCTime
timestampParse_ = timestampMsToUTCTime_ . ((Txt.pack . show) :: (Integer -> Text))

{-
_UTCTimeToTimestampMs :: UTCTime -> Text
_UTCTimeToTimestampMs = undefined
-}

class (FromJSON aes, ToJSON aes) => BitXRecordConvert rec aes | rec -> aes where
    aesToRec :: aes -> rec
    recToAes :: rec -> aes

    recToAes = undefined

-------------------------------------------- Ticker type -------------------------------------------

data Ticker_ = Ticker_
    { ticker'timestamp :: UTCTime
    , ticker'bid :: Decimal
    , ticker'ask :: Decimal
    , ticker'last :: Decimal
    , ticker'rolling24HourVolume :: Decimal
    , ticker'pair :: CcyPair
    } deriving (Show, Read)

instance FromJSON Ticker_ where
    parseJSON (Object v) =
        Ticker_ <$>
        liftM timestampParse_ (v .: "timestamp")
        <*> liftM read (v .: "bid")
        <*> liftM read (v .: "ask")
        <*> liftM read (v .: "last_trade")
        <*> liftM read (v .: "rolling_24_hour_volume")
        <*> (v .: "pair")
    parseJSON _ = mempty


instance ToJSON Ticker_ where
    toJSON _ = Null

tickerConverter_ :: Ticker_ -> Ticker
tickerConverter_ (Ticker_ ticker''timestamp ticker''bid ticker''ask ticker''lastTrade
        ticker''rolling24HourVolume ticker''pair) =
    [record| {timestamp = ticker''timestamp,
              bid = ticker''bid,
              ask = ticker''ask,
              lastTrade = ticker''lastTrade,
              rolling24HourVolume = ticker''rolling24HourVolume,
              pair = ticker''pair} |]

instance BitXRecordConvert Ticker Ticker_ where
    aesToRec = tickerConverter_

-------------------------------------------- BitXError type ----------------------------------------

data BitXError_= BitXError_
    { bitXError'error :: Text,
      bitXError'error_code :: Text
    } deriving (Show, Read)

$(AesTH.deriveJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = chopUpToPrime} ''BitXError_)

bitXErrorConverter_ :: BitXError_ -> BitXError
bitXErrorConverter_ (BitXError_ bitXError''error bitXError''error_code) =
    [record| {error = bitXError''error,
              errorCode = bitXError''error_code} |]

instance BitXRecordConvert BitXError BitXError_ where
    aesToRec = bitXErrorConverter_

-------------------------------------------- Order type --------------------------------------------

data Order_ = Order_
    { order'volume :: Decimal,
      order'price :: Decimal
    } deriving (Show, Read)

instance FromJSON Order_ where
    parseJSON (Object v) =
        Order_ <$>
        liftM read (v .: "volume")
        <*> liftM read (v .: "price")
    parseJSON _ = mempty

instance ToJSON Order_ where
    toJSON _ = Null

orderConverter_ :: Order_ -> Order
orderConverter_ (Order_ order''volume order''price) =
    [record| {volume = order''volume,
              price = order''price} |]

instance BitXRecordConvert Order Order_ where
    aesToRec = orderConverter_

-------------------------------------------- Orderbook type ----------------------------------------

data Orderbook_ = Orderbook_
    { orderbook'timestamp :: UTCTime,
      orderbook'bids :: [Bid_],
      orderbook'asks :: [Ask_]
    } deriving (Show, Read)

type Bid_ = Order_
type Ask_ = Order_

instance FromJSON Orderbook_ where
    parseJSON (Object v) =
        Orderbook_ <$>
        liftM timestampParse_ (v .: "timestamp")
        <*> (v .: "bids")
        <*> (v .: "asks")
    parseJSON _ = mempty

instance ToJSON Orderbook_ where
    toJSON _ = Null

orderbookConverter_ :: Orderbook_ -> Orderbook
orderbookConverter_ (Orderbook_ orderbook''timestamp orderbook''bids orderbook''asks) =
    [record| {timestamp = orderbook''timestamp,
              bids = map orderConverter_ orderbook''bids,
              asks = map orderConverter_ orderbook''asks} |]

instance BitXRecordConvert Orderbook Orderbook_ where
    aesToRec = orderbookConverter_

-------------------------------------------- Trade type --------------------------------------------

data Trade_ = Trade_
    { trade'volume :: Decimal
    , trade'timestamp :: UTCTime
    , trade'price :: Decimal
    } deriving (Show, Read)


instance FromJSON Trade_ where
    parseJSON (Object v) =
        Trade_ <$>
        liftM read (v .: "volume")
        <*> liftM timestampParse_ (v .: "timestamp")
        <*> liftM read (v .: "price")
    parseJSON _ = mempty

instance ToJSON Trade_ where
    toJSON _ = Null

tradeConverter_ :: Trade_ -> Trade
tradeConverter_ (Trade_ trade''volume trade''timestamp trade''price) =
    [record| {volume = trade''volume,
              timestamp = trade''timestamp,
              price = trade''price} |]

instance BitXRecordConvert Trade Trade_ where
    aesToRec = tradeConverter_

----------------------------------------- PublicTrades type ----------------------------------------

data PublicTrades_ = PublicTrades_
    { publicTrades'trades :: [Trade_]
    , publicTrades'currency :: Text
    } deriving (Show, Read)

$(AesTH.deriveJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = chopUpToPrime}
    ''PublicTrades_)

publicTradesConverter_ :: PublicTrades_ -> PublicTrades
publicTradesConverter_ (PublicTrades_ publicTrades''trades publicTrades''currency) =
    [record| {trades = map tradeConverter_ publicTrades''trades,
              currency = publicTrades''currency} |]

instance BitXRecordConvert PublicTrades PublicTrades_ where
    aesToRec = publicTradesConverter_

-------------------------------------------- BitXAuth type -----------------------------------------

data BitXAuth_ = BitXAuth_
    { bitXAuth'id :: Text
    , bitXAuth'secret :: Text
    } deriving (Show, Read)

$(AesTH.deriveJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = chopUpToPrime} ''BitXAuth_)

{-
bitXAuthConverter_ :: BitXAuth_ -> BitXAuth
bitXAuthConverter_ (BitXAuth_ bitXAuth''id bitXAuth''secret) =
    [record| {id = bitXAuth''id,
              secret = bitXAuth''secret} |]
-}

bitXAuthConverterRev_ :: BitXAuth -> BitXAuth_
bitXAuthConverterRev_ bxa =
    BitXAuth_ (view [lens| id |] bxa) (view [lens| secret |] bxa)

instance BitXRecordConvert BitXAuth BitXAuth_ where
    aesToRec = undefined
    recToAes = bitXAuthConverterRev_

------------------------------------------ PrivateOrder type ---------------------------------------

data PrivateOrder_ = PrivateOrder_
    { privateOrder'base :: Decimal
    , privateOrder'counter :: Decimal
    , privateOrder'creation_timestamp :: UTCTime
    , privateOrder'expiration_timestamp :: UTCTime
    , privateOrder'fee_base :: Decimal
    , privateOrder'fee_counter :: Decimal
    , privateOrder'limit_price :: Decimal
    , privateOrder'limit_volume :: Decimal
    , privateOrder'order_id :: OrderID
    , privateOrder'pair :: CcyPair
    , privateOrder'state :: OrderStatus
    , privateOrder'type :: OrderType
    } deriving (Show, Read)

instance FromJSON PrivateOrder_ where
    parseJSON (Object v) =
        PrivateOrder_ <$>
        liftM read (v .: "base")
        <*> liftM read (v .: "counter")
        <*> liftM timestampParse_ (v .: "creation_timestamp")
        <*> liftM timestampParse_ (v .: "expiration_timestamp")
        <*> liftM read (v .: "fee_base")
        <*> liftM read (v .: "fee_counter")
        <*> liftM read (v .: "limit_price")
        <*> liftM read (v .: "limit_volume")
        <*> (v .: "order_id")
        <*> (v .: "pair")
        <*> (v .: "state")
        <*> (v .: "type")
    parseJSON _ = mempty

instance ToJSON PrivateOrder_ where
    toJSON _ = Null

privateOrderConverter_ :: PrivateOrder_ -> PrivateOrder
privateOrderConverter_ (PrivateOrder_ privateOrder''base privateOrder''counter
        privateOrder''creation_timestamp privateOrder''expiration_timestamp privateOrder''fee_base
        privateOrder''fee_counter privateOrder''limit_price privateOrder''limit_volume
        privateOrder''order_id privateOrder''pair privateOrder''state privateOrder''type) =
    [record| {base = privateOrder''base,
              counter = privateOrder''counter,
              creationTimestamp = privateOrder''creation_timestamp,
              expirationTimestamp = privateOrder''expiration_timestamp,
              feeBase = privateOrder''fee_base,
              feeCounter = privateOrder''fee_counter,
              limitPrice = privateOrder''limit_price,
              limitVolume = privateOrder''limit_volume,
              orderID = privateOrder''order_id,
              pair = privateOrder''pair,
              state = privateOrder''state,
              orderType = privateOrder''type} |]

instance BitXRecordConvert PrivateOrder PrivateOrder_ where
    aesToRec = privateOrderConverter_

------------------------------------------ OrderRequest type ---------------------------------------

data OrderRequest_ = OrderRequest_
    { orderRequest'pair :: CcyPair
    , orderRequest'type :: OrderType
    , orderRequest'volume :: Decimal
    , orderRequest'price :: Decimal
    } deriving (Show, Read)

instance ToJSON OrderRequest_ where
    toJSON (OrderRequest_ orderRequest''pair orderRequest''type orderRequest''volume
            orderRequest''price) =
        object [    "pair" .= show orderRequest''pair
               ,    "type" .= show orderRequest''type
               , "volumne" .= show orderRequest''volume
               ,   "price" .= show orderRequest''price
               ]

instance FromJSON OrderRequest_ where
    parseJSON _ = mempty

orderRequestConverterRev_ :: OrderRequest -> OrderRequest_
orderRequestConverterRev_ oreq =
    OrderRequest_ (view [lens| pair |] oreq) (view [lens| requestType |] oreq)
        (view [lens| volume |] oreq) (view [lens| price |] oreq)

instance BitXRecordConvert OrderRequest OrderRequest_ where
    aesToRec = undefined
    recToAes = orderRequestConverterRev_

--------------------------------------------- Tickers type -----------------------------------------

data Tickers_ = Tickers_
    { tickers'tickers :: [Ticker_]
    } deriving (Show, Read)

$(AesTH.deriveJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = chopUpToPrime} ''Tickers_)

tickersConverter_ :: Tickers_ -> Tickers
tickersConverter_ (Tickers_ tickers''tickers) =
    [record| {tickers = map tickerConverter_ tickers''tickers} |]

instance BitXRecordConvert Tickers Tickers_ where
    aesToRec = tickersConverter_
