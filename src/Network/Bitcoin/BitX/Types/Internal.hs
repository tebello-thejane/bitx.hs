{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, MultiParamTypeClasses,
    FunctionalDependencies, FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}

module Network.Bitcoin.BitX.Types.Internal
    (
    BitXAesRecordConvert(..),
    Ticker_(..),
    BitXError_(..),
    bitXErrorConverter_,
    Tickers_(..),
    --BitXAuth_(..),
    Order_(..),
    POSTEncodeable(..)
    )
where

import Network.Bitcoin.BitX.Types
import Data.Aeson (FromJSON(..), parseJSON, (.:), Value(..))
import qualified Data.Aeson.TH as AesTH
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as Txt
import Data.Text (Text)
import Control.Applicative ((<$>), (<*>))
import Data.Time.Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Control.Monad (liftM)
import Record
import Record.Lens (view)
import Data.Monoid (mempty)
import Data.Decimal
import Data.ByteString (ByteString)
import Data.List.Split (splitOn)

timestampParse_ :: Integer -> UTCTime
timestampParse_ = posixSecondsToUTCTime
        . fromRational . toRational
        . ( / 1000)
        . (fromIntegral :: Integer -> Decimal)
     where
         div100Rev st = (take 3 st) ++ "." ++ (drop 3 st)

class (FromJSON aes) => BitXAesRecordConvert rec aes | rec -> aes where
    aesToRec :: aes -> rec

class POSTEncodeable rec where
    postEncode :: rec -> [(ByteString, ByteString)]

showableToBytestring :: (Show a) => a -> ByteString
showableToBytestring = Txt.encodeUtf8 . Txt.pack . show

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

tickerConverter_ :: Ticker_ -> Ticker
tickerConverter_ (Ticker_ ticker''timestamp ticker''bid ticker''ask ticker''lastTrade
        ticker''rolling24HourVolume ticker''pair) =
    [record| {timestamp = ticker''timestamp,
              bid = ticker''bid,
              ask = ticker''ask,
              lastTrade = ticker''lastTrade,
              rolling24HourVolume = ticker''rolling24HourVolume,
              pair = ticker''pair} |]

instance BitXAesRecordConvert Ticker Ticker_ where
    aesToRec = tickerConverter_

-------------------------------------------- BitXError type ----------------------------------------

data BitXError_= BitXError_
    { bitXError'error :: Text,
      bitXError'error_code :: Text
    } deriving (Show, Read)

$(AesTH.deriveJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"} ''BitXError_)

bitXErrorConverter_ :: BitXError_ -> BitXError
bitXErrorConverter_ (BitXError_ bitXError''error bitXError''error_code) =
    [record| {error = bitXError''error,
              errorCode = bitXError''error_code} |]

instance BitXAesRecordConvert BitXError BitXError_ where
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

orderConverter_ :: Order_ -> Order
orderConverter_ (Order_ order''volume order''price) =
    [record| {volume = order''volume,
              price = order''price} |]

instance BitXAesRecordConvert Order Order_ where
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

orderbookConverter_ :: Orderbook_ -> Orderbook
orderbookConverter_ (Orderbook_ orderbook''timestamp orderbook''bids orderbook''asks) =
    [record| {timestamp = orderbook''timestamp,
              bids = map orderConverter_ orderbook''bids,
              asks = map orderConverter_ orderbook''asks} |]

instance BitXAesRecordConvert Orderbook Orderbook_ where
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

tradeConverter_ :: Trade_ -> Trade
tradeConverter_ (Trade_ trade''volume trade''timestamp trade''price) =
    [record| {volume = trade''volume,
              timestamp = trade''timestamp,
              price = trade''price} |]

instance BitXAesRecordConvert Trade Trade_ where
    aesToRec = tradeConverter_

----------------------------------------- PublicTrades type ----------------------------------------

data PublicTrades_ = PublicTrades_
    { publicTrades'trades :: [Trade_]
    , publicTrades'currency :: Text
    } deriving (Show, Read)

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''PublicTrades_)

publicTradesConverter_ :: PublicTrades_ -> PublicTrades
publicTradesConverter_ (PublicTrades_ publicTrades''trades publicTrades''currency) =
    [record| {trades = map tradeConverter_ publicTrades''trades,
              currency = publicTrades''currency} |]

instance BitXAesRecordConvert PublicTrades PublicTrades_ where
    aesToRec = publicTradesConverter_

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

instance BitXAesRecordConvert PrivateOrder PrivateOrder_ where
    aesToRec = privateOrderConverter_

------------------------------------------ OrderRequest type ---------------------------------------

instance POSTEncodeable OrderRequest where
    postEncode oreq =
        [("pair", showableToBytestring (view [lens| pair |] oreq)),
         ("type", showableToBytestring (view [lens| requestType |] oreq)),
         ("volume", showableToBytestring (view [lens| volume |] oreq)),
         ("price", showableToBytestring (view [lens| price |] oreq))]

--------------------------------------------- Tickers type -----------------------------------------

data Tickers_ = Tickers_
    { tickers'tickers :: [Ticker_]
    } deriving (Show, Read)

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Tickers_)

tickersConverter_ :: Tickers_ -> Tickers
tickersConverter_ (Tickers_ tickers''tickers) =
    [record| {tickers = map tickerConverter_ tickers''tickers} |]

instance BitXAesRecordConvert Tickers Tickers_ where
    aesToRec = tickersConverter_

------------------------------------------ PrivateOrders type --------------------------------------

data PrivateOrders_ = PrivateOrders_
    {privateOrders'orders :: [PrivateOrder_]
    } deriving (Read, Show)

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''PrivateOrders_)

privateOrdersConverter_ :: PrivateOrders_ -> PrivateOrders
privateOrdersConverter_ (PrivateOrders_ privateOrders''orders) =
    [record| {orders = map privateOrderConverter_ privateOrders''orders} |]

instance BitXAesRecordConvert PrivateOrders PrivateOrders_ where
    aesToRec = privateOrdersConverter_

-------------------------------------------- OrderIDRec type ---------------------------------------

data OrderIDRec_ = OrderIDRec_
    { orderIDResponse'order_id :: OrderID
    } deriving (Show, Read)

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''OrderIDRec_)

orderIDRecConverter_ :: OrderIDRec_ -> OrderID
orderIDRecConverter_ (OrderIDRec_ orderIDResponse''order_id) =
    orderIDResponse''order_id

instance BitXAesRecordConvert OrderID OrderIDRec_ where
    aesToRec = orderIDRecConverter_

instance POSTEncodeable OrderID where
    postEncode oid =
        [("order_id", showableToBytestring oid)]

----------------------------------------- StopOrderSuccess type ------------------------------------

data StopOrderSuccess_ = StopOrderSuccess_
    { stopOrderSuccess'success :: Bool
    } deriving (Show, Read)

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''StopOrderSuccess_)

stopOrderSuccessConverter_ :: StopOrderSuccess_ -> StopOrderSuccess
stopOrderSuccessConverter_ (StopOrderSuccess_ stopOrderSuccess''success) =
    stopOrderSuccess''success

instance BitXAesRecordConvert StopOrderSuccess StopOrderSuccess_ where
    aesToRec = stopOrderSuccessConverter_

------------------------------------- PrivateOrderWithTrades type ----------------------------------

data PrivateOrderWithTrades_ = PrivateOrderWithTrades_
    { privateOrderWithTrades'base :: Decimal
    , privateOrderWithTrades'counter :: Decimal
    , privateOrderWithTrades'creation_timestamp :: UTCTime
    , privateOrderWithTrades'expiration_timestamp :: UTCTime
    , privateOrderWithTrades'fee_base :: Decimal
    , privateOrderWithTrades'fee_counter :: Decimal
    , privateOrderWithTrades'limit_price :: Decimal
    , privateOrderWithTrades'limit_volume :: Decimal
    , privateOrderWithTrades'order_id :: OrderID
    , privateOrderWithTrades'pair :: CcyPair
    , privateOrderWithTrades'state :: OrderStatus
    , privateOrderWithTrades'type :: OrderType
    , privateOrderWithTrades'trades :: [Trade_]
    } deriving (Show, Read)

instance FromJSON PrivateOrderWithTrades_ where
    parseJSON (Object v) =
        PrivateOrderWithTrades_ <$>
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
        <*> (v .: "trades")
    parseJSON _ = mempty

privateOrderWithTradesConverter_ :: PrivateOrderWithTrades_ -> PrivateOrderWithTrades
privateOrderWithTradesConverter_ (PrivateOrderWithTrades_ privateOrder''base privateOrder''counter
        privateOrder''creation_timestamp privateOrder''expiration_timestamp privateOrder''fee_base
        privateOrder''fee_counter privateOrder''limit_price privateOrder''limit_volume
        privateOrder''order_id privateOrder''pair privateOrder''state privateOrder''type
        privateOrderWithTrades''trades) =
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
              orderType = privateOrder''type,
              trades = map tradeConverter_ privateOrderWithTrades''trades} |]

instance BitXAesRecordConvert PrivateOrderWithTrades PrivateOrderWithTrades_ where
    aesToRec = privateOrderWithTradesConverter_