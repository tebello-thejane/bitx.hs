{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, MultiParamTypeClasses,
    FunctionalDependencies, FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-orphans #-}

module Network.Bitcoin.BitX.Types.Internal
    (
    BitXAesRecordConvert(..),
    Ticker_(..),
    BitXError_(..),
    Tickers_(..),
    Order_(..),
    POSTEncodeable(..)
    )
where

import Network.Bitcoin.BitX.Types
import Data.Aeson (FromJSON(..), parseJSON, (.:), Value(..), ToJSON(..))
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
import Data.Decimal (Decimal)
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

instance FromJSON Decimal where
   parseJSON (String x) = return . read . Txt.unpack $ x
   parseJSON _          = mempty

instance ToJSON Decimal where
    toJSON x = String . Txt.pack . show $ x

-------------------------------------------- Ticker type -------------------------------------------

data Ticker_ = Ticker_
    { ticker'timestamp :: UTCTime
    , ticker'bid :: Decimal
    , ticker'ask :: Decimal
    , ticker'last :: Decimal
    , ticker'rolling24HourVolume :: Decimal
    , ticker'pair :: CcyPair
    }

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

instance BitXAesRecordConvert Ticker Ticker_ where
    aesToRec (Ticker_ ticker''timestamp ticker''bid ticker''ask ticker''lastTrade
            ticker''rolling24HourVolume ticker''pair) =
        [record| {timestamp = ticker''timestamp,
                  bid = ticker''bid,
                  ask = ticker''ask,
                  lastTrade = ticker''lastTrade,
                  rolling24HourVolume = ticker''rolling24HourVolume,
                  pair = ticker''pair} |]

-------------------------------------------- BitXError type ----------------------------------------

data BitXError_= BitXError_
    { bitXError'error :: Text,
      bitXError'error_code :: Text
    }

$(AesTH.deriveJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"} ''BitXError_)

instance BitXAesRecordConvert BitXError BitXError_ where
    aesToRec (BitXError_ bitXError''error bitXError''error_code) =
        [record| {error = bitXError''error,
              errorCode = bitXError''error_code} |]

-------------------------------------------- Order type --------------------------------------------

data Order_ = Order_
    { order'volume :: Decimal,
      order'price :: Decimal
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"} ''Order_)

instance BitXAesRecordConvert Order Order_ where
    aesToRec (Order_ order''volume order''price) =
        [record| {volume = order''volume,
              price = order''price} |]
-------------------------------------------- Orderbook type ----------------------------------------

data Orderbook_ = Orderbook_
    { orderbook'timestamp :: UTCTime,
      orderbook'bids :: [Bid_],
      orderbook'asks :: [Ask_]
    }

type Bid_ = Order_
type Ask_ = Order_

instance FromJSON Orderbook_ where
    parseJSON (Object v) =
        Orderbook_ <$>
        liftM timestampParse_ (v .: "timestamp")
        <*> (v .: "bids")
        <*> (v .: "asks")
    parseJSON _ = mempty

instance BitXAesRecordConvert Orderbook Orderbook_ where
    aesToRec (Orderbook_ orderbook''timestamp orderbook''bids orderbook''asks) =
        [record| {timestamp = orderbook''timestamp,
                  bids = map aesToRec orderbook''bids,
                  asks = map aesToRec orderbook''asks} |]

-------------------------------------------- Trade type --------------------------------------------

data Trade_ = Trade_
    { trade'volume :: Decimal
    , trade'timestamp :: UTCTime
    , trade'price :: Decimal
    }

instance FromJSON Trade_ where
    parseJSON (Object v) =
        Trade_ <$>
        liftM read (v .: "volume")
        <*> liftM timestampParse_ (v .: "timestamp")
        <*> liftM read (v .: "price")
    parseJSON _ = mempty

instance BitXAesRecordConvert Trade Trade_ where
    aesToRec (Trade_ trade''volume trade''timestamp trade''price) =
        [record| {volume = trade''volume,
              timestamp = trade''timestamp,
              price = trade''price} |]

----------------------------------------- PublicTrades type ----------------------------------------

data PublicTrades_ = PublicTrades_
    { publicTrades'trades :: [Trade_]
    , publicTrades'currency :: Text
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''PublicTrades_)

instance BitXAesRecordConvert PublicTrades PublicTrades_ where
    aesToRec (PublicTrades_ publicTrades''trades publicTrades''currency) =
        [record| {trades = map aesToRec publicTrades''trades,
              currency = publicTrades''currency} |]

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
    , privateOrder'state :: RequestStatus
    , privateOrder'type :: OrderType
    }

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

instance BitXAesRecordConvert PrivateOrder PrivateOrder_ where
    aesToRec (PrivateOrder_ privateOrder''base privateOrder''counter
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
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Tickers_)

instance BitXAesRecordConvert Tickers Tickers_ where
    aesToRec (Tickers_ tickers''tickers) =
        [record| {tickers = map aesToRec tickers''tickers} |]

------------------------------------------ PrivateOrders type --------------------------------------

data PrivateOrders_ = PrivateOrders_
    {privateOrders'orders :: [PrivateOrder_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''PrivateOrders_)

instance BitXAesRecordConvert PrivateOrders PrivateOrders_ where
    aesToRec (PrivateOrders_ privateOrders''orders) =
        [record| {orders = map aesToRec privateOrders''orders} |]

-------------------------------------------- OrderIDRec type ---------------------------------------

data OrderIDRec_ = OrderIDRec_
    { orderIDResponse'order_id :: OrderID
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''OrderIDRec_)

instance BitXAesRecordConvert OrderID OrderIDRec_ where
    aesToRec (OrderIDRec_ orderIDResponse''order_id) =
        orderIDResponse''order_id

instance POSTEncodeable OrderID where
    postEncode oid =
        [("order_id", showableToBytestring oid)]

----------------------------------------- StopOrderSuccess type ------------------------------------

data StopOrderSuccess_ = StopOrderSuccess_
    { stopOrderSuccess'success :: Bool
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''StopOrderSuccess_)

instance BitXAesRecordConvert StopOrderSuccess StopOrderSuccess_ where
    aesToRec (StopOrderSuccess_ stopOrderSuccess''success) =
        stopOrderSuccess''success

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
    , privateOrderWithTrades'state :: RequestStatus
    , privateOrderWithTrades'type :: OrderType
    , privateOrderWithTrades'trades :: [Trade_]
    }

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

instance BitXAesRecordConvert PrivateOrderWithTrades PrivateOrderWithTrades_ where
    aesToRec (PrivateOrderWithTrades_ privateOrder''base privateOrder''counter
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
                  trades = map aesToRec privateOrderWithTrades''trades} |]

-------------------------------------------- Balance type ------------------------------------------

data Balance_ = Balance_
    { balance'account_id :: AccountID
    , balance'asset :: Asset
    , balance'balance :: Decimal
    , balance'reserved :: Decimal
    , balance'unconfirmed :: Decimal
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Balance_)

instance BitXAesRecordConvert Balance Balance_ where
    aesToRec (Balance_ balance''account_id balance''asset balance''balance balance''reserved
            balance''unconfirmed) =
        [record| {accountID = balance''account_id,
                  asset = balance''asset,
                  balance = balance''balance,
                  reserved = balance''reserved,
                  unconfirmed = balance''unconfirmed} |]
-------------------------------------------- Balances type -----------------------------------------

data Balances_ = Balances_
    {balances'balances :: [Balance_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Balances_)

instance BitXAesRecordConvert Balances Balances_ where
    aesToRec (Balances_ balances''balances) =
        [record| {balances = map aesToRec balances''balances} |]
----------------------------------------- FundingAddress type --------------------------------------

data FundingAddress_ = FundingAddress_
    { fundingAdress'asset :: Asset
    , fundingAdress'address :: Text
    , fundingAdress'total_received :: Decimal
    , fundingAdress'total_unconfirmed :: Decimal
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''FundingAddress_)

instance BitXAesRecordConvert FundingAddress FundingAddress_ where
    aesToRec (FundingAddress_ fundingAdress''asset fundingAdress''address
            fundingAdress''total_received fundingAdress''total_unconfirmed) =
        [record| {asset = fundingAdress''asset,
                  address = fundingAdress''address,
                  totalReceived = fundingAdress''total_received,
                  totalUnconfirmed = fundingAdress''total_unconfirmed} |]

--------------------------------------------- Asset type -------------------------------------------

instance POSTEncodeable Asset where
    postEncode asset =
        [("asset", showableToBytestring asset)]

-------------------------------------- WithdrawalRequest type --------------------------------------

data WithdrawalRequest_ = WithdrawalRequest_
    { withdrawalRequest'status :: RequestStatus
    , withdrawalRequest'id :: Text
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''WithdrawalRequest_)

instance BitXAesRecordConvert WithdrawalRequest WithdrawalRequest_ where
    aesToRec (WithdrawalRequest_ withdrawalRequest''status withdrawalRequest''id) =
        [record| {status = withdrawalRequest''status,
                  id = withdrawalRequest''id} |]

-------------------------------------- WithdrawalRequests type -------------------------------------

data WithdrawalRequests_ = WithdrawalRequests_
    { withdrawalRequests'withdrawals :: [WithdrawalRequest_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''WithdrawalRequests_)

instance BitXAesRecordConvert WithdrawalRequests WithdrawalRequests_ where
    aesToRec (WithdrawalRequests_ withdrawalRequests''withdrawals) =
        [record| {withdrawalRequests = map aesToRec withdrawalRequests''withdrawals} |]

----------------------------------------- NewWithdrawal type ---------------------------------------

data NewWithdrawal_ = NewWithdrawal_
    { newWithdrawal'type :: WithdrawalType
    , newWithdrawal'amount :: Decimal
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''NewWithdrawal_)

instance POSTEncodeable NewWithdrawal where
    postEncode nwthd =
        [("type", showableToBytestring (view [lens| withdrawalType |] nwthd)),
         ("amount", showableToBytestring (view [lens| amount |] nwthd))]
