{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, MultiParamTypeClasses,
    FunctionalDependencies, FlexibleInstances, DataKinds, FlexibleContexts, CPP #-}

{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}

module Network.Bitcoin.BitX.Types.Internal
    (
    BitXAesRecordConvert(..),
    POSTEncodeable(..)
    )
where

import Network.Bitcoin.BitX.Types
import Data.Aeson (FromJSON(..), parseJSON, Value(..))
import qualified Data.Aeson.TH as AesTH
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as Txt
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Record
import Record.Lens (view)
#if MIN_VERSION_base(4,8,0)
-- base 4.8 re-exports Monoid and its functions/constants
#else
import Data.Monoid (mempty)
#endif
import Data.Decimal (Decimal)
import Data.ByteString (ByteString)
import Data.List.Split (splitOn)

timestampParse_ :: Integer -> UTCTime
timestampParse_ = posixSecondsToUTCTime
        . fromRational . toRational
        . ( / 1000)
        . (fromIntegral :: Integer -> Decimal)

class (FromJSON aes) => BitXAesRecordConvert rec aes | rec -> aes where
    aesToRec :: aes -> rec

class POSTEncodeable rec where
    postEncode :: rec -> [(ByteString, ByteString)]


showableToBytestring :: (Show a) => a -> ByteString
showableToBytestring = Txt.encodeUtf8 . Txt.pack . show

-- | Wrapper around Decimal and FromJSON instance, to facilitate automatic JSON instances

newtype QuotedDecimal = QuotedDecimal Decimal deriving (Read, Show)

instance FromJSON QuotedDecimal where
   parseJSON (String x) = return . QuotedDecimal . read . Txt.unpack $ x
   parseJSON _          = mempty

qdToDecimal :: QuotedDecimal -> Decimal
qdToDecimal (QuotedDecimal dec) = dec

-- | Wrapper around UTCTime and FromJSON instance, to facilitate automatic JSON instances

newtype TimestampMS = TimestampMS Integer deriving (Read, Show)

instance FromJSON TimestampMS where
   parseJSON (Number x) = return . TimestampMS . round $ x
   parseJSON _          = mempty

tsmsToUTCTime :: TimestampMS -> UTCTime
tsmsToUTCTime (TimestampMS ms) = timestampParse_ ms

-------------------------------------------- Ticker type -------------------------------------------

data Ticker_ = Ticker_
    { ticker'timestamp :: TimestampMS
    , ticker'bid :: QuotedDecimal
    , ticker'ask :: QuotedDecimal
    , ticker'last_trade :: QuotedDecimal
    , ticker'rolling_24_hour_volume :: QuotedDecimal
    , ticker'pair :: CcyPair
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Ticker_)

instance BitXAesRecordConvert Ticker Ticker_ where
    aesToRec (Ticker_ ticker''timestamp ticker''bid ticker''ask ticker''lastTrade
            ticker''rolling24HourVolume ticker''pair) =
        [record| {timestamp = tsmsToUTCTime ticker''timestamp,
                  bid = qdToDecimal ticker''bid,
                  ask = qdToDecimal ticker''ask,
                  lastTrade = qdToDecimal ticker''lastTrade,
                  rolling24HourVolume = qdToDecimal ticker''rolling24HourVolume,
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
    { order'volume :: QuotedDecimal,
      order'price :: QuotedDecimal
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"} ''Order_)

instance BitXAesRecordConvert Order Order_ where
    aesToRec (Order_ order''volume order''price) =
        [record| {volume =  qdToDecimal order''volume,
              price = qdToDecimal order''price} |]

-------------------------------------------- Orderbook type ----------------------------------------

data Orderbook_ = Orderbook_
    { orderbook'timestamp :: TimestampMS,
      orderbook'bids :: [Bid_],
      orderbook'asks :: [Ask_]
    }

type Bid_ = Order_
type Ask_ = Order_

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Orderbook_)

instance BitXAesRecordConvert Orderbook Orderbook_ where
    aesToRec (Orderbook_ orderbook''timestamp orderbook''bids orderbook''asks) =
        [record| {timestamp = tsmsToUTCTime orderbook''timestamp,
                  bids = map aesToRec orderbook''bids,
                  asks = map aesToRec orderbook''asks} |]

-------------------------------------------- Trade type --------------------------------------------

data Trade_ = Trade_
    { trade'volume :: QuotedDecimal
    , trade'timestamp :: TimestampMS
    , trade'price :: QuotedDecimal
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Trade_)

instance BitXAesRecordConvert Trade Trade_ where
    aesToRec (Trade_ trade''volume trade''timestamp trade''price) =
        [record| {volume = qdToDecimal trade''volume,
              timestamp = tsmsToUTCTime trade''timestamp,
              price = qdToDecimal trade''price} |]

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
    { privateOrder'base :: QuotedDecimal
    , privateOrder'counter :: QuotedDecimal
    , privateOrder'creation_timestamp :: TimestampMS
    , privateOrder'expiration_timestamp :: TimestampMS
    , privateOrder'fee_base :: QuotedDecimal
    , privateOrder'fee_counter :: QuotedDecimal
    , privateOrder'limit_price :: QuotedDecimal
    , privateOrder'limit_volume :: QuotedDecimal
    , privateOrder'order_id :: OrderID
    , privateOrder'pair :: CcyPair
    , privateOrder'state :: RequestStatus
    , privateOrder'type :: OrderType
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''PrivateOrder_)

instance BitXAesRecordConvert PrivateOrder PrivateOrder_ where
    aesToRec (PrivateOrder_ privateOrder''base privateOrder''counter
          privateOrder''creation_timestamp privateOrder''expiration_timestamp privateOrder''fee_base
          privateOrder''fee_counter privateOrder''limit_price privateOrder''limit_volume
          privateOrder''order_id privateOrder''pair privateOrder''state privateOrder''type) =
        [record| {base = qdToDecimal privateOrder''base,
                  counter = qdToDecimal privateOrder''counter,
                  creationTimestamp = tsmsToUTCTime privateOrder''creation_timestamp,
                  expirationTimestamp = tsmsToUTCTime privateOrder''expiration_timestamp,
                  feeBase = qdToDecimal privateOrder''fee_base,
                  feeCounter = qdToDecimal privateOrder''fee_counter,
                  limitPrice = qdToDecimal privateOrder''limit_price,
                  limitVolume = qdToDecimal privateOrder''limit_volume,
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
    { privateOrderWithTrades'base :: QuotedDecimal
    , privateOrderWithTrades'counter :: QuotedDecimal
    , privateOrderWithTrades'creation_timestamp :: TimestampMS
    , privateOrderWithTrades'expiration_timestamp :: TimestampMS
    , privateOrderWithTrades'fee_base :: QuotedDecimal
    , privateOrderWithTrades'fee_counter :: QuotedDecimal
    , privateOrderWithTrades'limit_price :: QuotedDecimal
    , privateOrderWithTrades'limit_volume :: QuotedDecimal
    , privateOrderWithTrades'order_id :: OrderID
    , privateOrderWithTrades'pair :: CcyPair
    , privateOrderWithTrades'state :: RequestStatus
    , privateOrderWithTrades'type :: OrderType
    , privateOrderWithTrades'trades :: [Trade_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''PrivateOrderWithTrades_)

instance BitXAesRecordConvert PrivateOrderWithTrades PrivateOrderWithTrades_ where
    aesToRec (PrivateOrderWithTrades_ privateOrder''base privateOrder''counter
            privateOrder''creation_timestamp privateOrder''expiration_timestamp privateOrder''fee_base
            privateOrder''fee_counter privateOrder''limit_price privateOrder''limit_volume
            privateOrder''order_id privateOrder''pair privateOrder''state privateOrder''type
            privateOrderWithTrades''trades) =
        [record| {base = qdToDecimal privateOrder''base,
                  counter = qdToDecimal privateOrder''counter,
                  creationTimestamp = tsmsToUTCTime privateOrder''creation_timestamp,
                  expirationTimestamp = tsmsToUTCTime privateOrder''expiration_timestamp,
                  feeBase = qdToDecimal privateOrder''fee_base,
                  feeCounter = qdToDecimal privateOrder''fee_counter,
                  limitPrice = qdToDecimal privateOrder''limit_price,
                  limitVolume = qdToDecimal privateOrder''limit_volume,
                  orderID = privateOrder''order_id,
                  pair = privateOrder''pair,
                  state = privateOrder''state,
                  orderType = privateOrder''type,
                  trades = map aesToRec privateOrderWithTrades''trades} |]

-------------------------------------------- Balance type ------------------------------------------

data Balance_ = Balance_
    { balance'account_id :: AccountID
    , balance'asset :: Asset
    , balance'balance :: QuotedDecimal
    , balance'reserved :: QuotedDecimal
    , balance'unconfirmed :: QuotedDecimal
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Balance_)

instance BitXAesRecordConvert Balance Balance_ where
    aesToRec (Balance_ balance''account_id balance''asset balance''balance balance''reserved
            balance''unconfirmed) =
        [record| {accountID = balance''account_id,
                  asset = balance''asset,
                  balance = qdToDecimal balance''balance,
                  reserved = qdToDecimal balance''reserved,
                  unconfirmed = qdToDecimal balance''unconfirmed} |]

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
    , fundingAdress'total_received :: QuotedDecimal
    , fundingAdress'total_unconfirmed :: QuotedDecimal
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''FundingAddress_)

instance BitXAesRecordConvert FundingAddress FundingAddress_ where
    aesToRec (FundingAddress_ fundingAdress''asset fundingAdress''address
            fundingAdress''total_received fundingAdress''total_unconfirmed) =
        [record| {asset = fundingAdress''asset,
                  address = fundingAdress''address,
                  totalReceived = qdToDecimal fundingAdress''total_received,
                  totalUnconfirmed = qdToDecimal fundingAdress''total_unconfirmed} |]

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
    , newWithdrawal'amount :: QuotedDecimal
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''NewWithdrawal_)

instance POSTEncodeable NewWithdrawal where
    postEncode nwthd =
        [("type", showableToBytestring (view [lens| withdrawalType |] nwthd)),
         ("amount", showableToBytestring (view [lens| amount |] nwthd))]
