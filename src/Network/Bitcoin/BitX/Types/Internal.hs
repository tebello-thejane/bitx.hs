{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, MultiParamTypeClasses,
    FunctionalDependencies, FlexibleInstances, DataKinds, CPP, RecordWildCards #-}

--{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Network.Bitcoin.BitX.Types.Internal
    (
    BitXAesRecordConvert(..),
    POSTEncodeable(..),
    --showableToBytestring_,
    Transaction_(..),
    pendingTransactionsToTransactions,
    PendingTransactions__
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
import Data.Scientific (Scientific)
--import Data.Scientific (Scientific)
import Data.ByteString (ByteString)
import Data.List.Split (splitOn)
#if MIN_VERSION_base(4,7,0)
import Data.Coerce
#endif

timestampParse_ :: Integer -> UTCTime
timestampParse_ = posixSecondsToUTCTime
    . realToFrac
    . ( / 1000)
    . (fromIntegral :: Integer -> Scientific)

class (FromJSON aes) => BitXAesRecordConvert rec aes | rec -> aes where
    aesToRec :: aes -> rec

class POSTEncodeable rec where
    postEncode :: rec -> [(ByteString, ByteString)]


showableToBytestring_ :: (Show a) => a -> ByteString
showableToBytestring_ = Txt.encodeUtf8 . Txt.pack . show

-- | Wrapper around Scientific and FromJSON instance, to facilitate automatic JSON instances

newtype QuotedScientific = QuotedScientific Scientific deriving (Read, Show)

instance FromJSON QuotedScientific where
   parseJSON (String x) = return . QuotedScientific . read . Txt.unpack $ x
   parseJSON (Number x) = return . QuotedScientific . read . show $ x
   parseJSON _          = mempty

--instance ToJSON QuotedScientific where
--    toJSON (QuotedScientific q) = Number . realToFrac $ q

qsToScientific :: QuotedScientific -> Scientific
#if MIN_VERSION_base(4,7,0)
qsToScientific = coerce
{-# INLINE qsToScientific #-}
#else
qsToScientific (QuotedScientific dec) = dec
#endif

-- | Wrapper around UTCTime and FromJSON instance, to facilitate automatic JSON instances

newtype TimestampMS = TimestampMS Integer deriving (Read, Show)

instance FromJSON TimestampMS where
   parseJSON (Number x) = return . TimestampMS . round $ x
   parseJSON _          = mempty

--instance ToJSON TimestampMS where
--    toJSON (TimestampMS t) = Number . fromIntegral $ t

tsmsToUTCTime :: TimestampMS -> UTCTime
tsmsToUTCTime (TimestampMS ms) = timestampParse_ ms


newtype OrderType_ = OrderType_ Text deriving (Read, Show)

instance FromJSON OrderType_ where
   parseJSON (String x) = return . OrderType_ $ x
   parseJSON _          = mempty

orderTypeParse :: OrderType_ -> OrderType
orderTypeParse (OrderType_ "BUY")  = BID
orderTypeParse (OrderType_ "BID")  = BID
orderTypeParse (OrderType_ "ASK")  = ASK
orderTypeParse (OrderType_ "SELL") = ASK
orderTypeParse (OrderType_    x  ) =
    error $ "Yet another surprise from the BitX API: unexpected OrderType " ++ Txt.unpack x


newtype RequestStatus_ = RequestStatus_ Text deriving (Read, Show)

instance FromJSON RequestStatus_ where
   parseJSON (String x) = return . RequestStatus_ $ x
   parseJSON _          = mempty

requestStatusParse :: RequestStatus_ -> RequestStatus
requestStatusParse (RequestStatus_ "PENDING")   = PENDING
requestStatusParse (RequestStatus_ "COMPLETE")  = COMPLETE
requestStatusParse (RequestStatus_ "COMPLETED") = COMPLETE
requestStatusParse (RequestStatus_ "CANCELLED") = CANCELLED
requestStatusParse (RequestStatus_      x     ) =
    error $ "Yet another surprise from the BitX API: unexpected RequestStatus " ++ Txt.unpack x

-------------------------------------------- Ticker type -------------------------------------------

data Ticker_ = Ticker_
    { ticker'timestamp :: TimestampMS
    , ticker'bid :: QuotedScientific
    , ticker'ask :: QuotedScientific
    , ticker'last_trade :: QuotedScientific
    , ticker'rolling_24_hour_volume :: QuotedScientific
    , ticker'pair :: CcyPair
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Ticker_)

instance BitXAesRecordConvert Ticker Ticker_ where
    aesToRec (Ticker_ {..}) =
        [record| {timestamp = tsmsToUTCTime ticker'timestamp,
                  bid = qsToScientific ticker'bid,
                  ask = qsToScientific ticker'ask,
                  lastTrade = qsToScientific ticker'last_trade,
                  rolling24HourVolume = qsToScientific ticker'rolling_24_hour_volume,
                  pair = ticker'pair} |]

--------------------------------------------- Tickers type -----------------------------------------

data Tickers_ = Tickers_
    { tickers'tickers :: [Ticker_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Tickers_)

instance BitXAesRecordConvert [Ticker] Tickers_ where
    aesToRec (Tickers_ {..}) =
        map aesToRec tickers'tickers

-------------------------------------------- BitXError type ----------------------------------------

data BitXError_= BitXError_
    { bitXError'error :: Text,
      bitXError'error_code :: Text
    } deriving (Show, Eq)

$(AesTH.deriveJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"} ''BitXError_)

instance BitXAesRecordConvert BitXError BitXError_ where
    aesToRec (BitXError_ {..}) =
        [record| {error = bitXError'error,
              errorCode = bitXError'error_code} |]

-------------------------------------------- Order type --------------------------------------------

data Order_ = Order_
    { order'volume :: QuotedScientific,
      order'price :: QuotedScientific
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"} ''Order_)

instance BitXAesRecordConvert Order Order_ where
    aesToRec (Order_ {..}) =
        [record| {volume = qsToScientific order'volume,
              price = qsToScientific order'price} |]

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
    aesToRec (Orderbook_ {..}) =
        [record| {timestamp = tsmsToUTCTime orderbook'timestamp,
                  bids = map aesToRec orderbook'bids,
                  asks = map aesToRec orderbook'asks} |]

-------------------------------------------- Trade type --------------------------------------------

data Trade_ = Trade_
    { trade'volume :: QuotedScientific
    , trade'timestamp :: TimestampMS
    , trade'price :: QuotedScientific
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Trade_)

instance BitXAesRecordConvert Trade Trade_ where
    aesToRec (Trade_ {..}) =
        [record| {volume = qsToScientific trade'volume,
              timestamp = tsmsToUTCTime trade'timestamp,
              price = qsToScientific trade'price} |]

----------------------------------------- PublicTrades type ----------------------------------------

data PublicTrades_ = PublicTrades_
    { publicTrades'trades :: [Trade_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''PublicTrades_)

instance BitXAesRecordConvert [Trade] PublicTrades_ where
    aesToRec (PublicTrades_ {..}) =
        map aesToRec publicTrades'trades

------------------------------------------ PrivateOrder type ---------------------------------------

data PrivateOrder_ = PrivateOrder_
    { privateOrder'base :: QuotedScientific
    , privateOrder'counter :: QuotedScientific
    , privateOrder'creation_timestamp :: TimestampMS
    , privateOrder'expiration_timestamp :: TimestampMS
    , privateOrder'fee_base :: QuotedScientific
    , privateOrder'fee_counter :: QuotedScientific
    , privateOrder'limit_price :: QuotedScientific
    , privateOrder'limit_volume :: QuotedScientific
    , privateOrder'order_id :: OrderID
    , privateOrder'pair :: CcyPair
    , privateOrder'state :: RequestStatus_
    , privateOrder'type :: OrderType_
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''PrivateOrder_)

instance BitXAesRecordConvert PrivateOrder PrivateOrder_ where
    aesToRec (PrivateOrder_ {..}) =
        [record| {base = qsToScientific privateOrder'base,
                  counter = qsToScientific privateOrder'counter,
                  creationTimestamp = tsmsToUTCTime privateOrder'creation_timestamp,
                  expirationTimestamp = tsmsToUTCTime privateOrder'expiration_timestamp,
                  feeBase = qsToScientific privateOrder'fee_base,
                  feeCounter = qsToScientific privateOrder'fee_counter,
                  limitPrice = qsToScientific privateOrder'limit_price,
                  limitVolume = qsToScientific privateOrder'limit_volume,
                  id = privateOrder'order_id,
                  pair = privateOrder'pair,
                  state = requestStatusParse privateOrder'state,
                  type = orderTypeParse privateOrder'type} |]

------------------------------------------ PrivateOrders type --------------------------------------

data PrivateOrders_ = PrivateOrders_
    {privateOrders'orders :: [PrivateOrder_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''PrivateOrders_)

instance BitXAesRecordConvert [PrivateOrder] PrivateOrders_ where
    aesToRec (PrivateOrders_ {..}) =
        map aesToRec privateOrders'orders

------------------------------------------ OrderRequest type ---------------------------------------

instance POSTEncodeable OrderRequest where
    postEncode oreq =
        [("pair", showableToBytestring_ (view [lens| pair |] oreq)),
         ("type", showableToBytestring_ (view [lens| type |] oreq)),
         ("volume", showableToBytestring_ (view [lens| volume |] oreq)),
         ("price", showableToBytestring_ (view [lens| price |] oreq))]

-------------------------------------------- OrderIDRec type ---------------------------------------

data OrderIDRec_ = OrderIDRec_
    { orderIDResponse'order_id :: OrderID
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''OrderIDRec_)

instance BitXAesRecordConvert OrderID OrderIDRec_ where
    aesToRec (OrderIDRec_ {..}) =
        orderIDResponse'order_id

instance POSTEncodeable OrderID where
    postEncode oid =
        [("order_id", Txt.encodeUtf8 oid)]

----------------------------------------- RequestSuccess type --------------------------------------

data RequestSuccess_ = RequestSuccess_
    { requestSuccess'success :: Bool
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''RequestSuccess_)

instance BitXAesRecordConvert RequestSuccess RequestSuccess_ where
    aesToRec (RequestSuccess_ {..}) =
        requestSuccess'success

------------------------------------- PrivateOrderWithTrades type ----------------------------------

data PrivateOrderWithTrades_ = PrivateOrderWithTrades_
    { privateOrderWithTrades'base :: QuotedScientific
    , privateOrderWithTrades'counter :: QuotedScientific
    , privateOrderWithTrades'creation_timestamp :: TimestampMS
    , privateOrderWithTrades'expiration_timestamp :: TimestampMS
    , privateOrderWithTrades'fee_base :: QuotedScientific
    , privateOrderWithTrades'fee_counter :: QuotedScientific
    , privateOrderWithTrades'limit_price :: QuotedScientific
    , privateOrderWithTrades'limit_volume :: QuotedScientific
    , privateOrderWithTrades'order_id :: OrderID
    , privateOrderWithTrades'pair :: CcyPair
    , privateOrderWithTrades'state :: RequestStatus_
    , privateOrderWithTrades'type :: OrderType_
    , privateOrderWithTrades'trades :: [Trade_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''PrivateOrderWithTrades_)

instance BitXAesRecordConvert PrivateOrderWithTrades PrivateOrderWithTrades_ where
    aesToRec (PrivateOrderWithTrades_ {..}) =
        [record| {base = qsToScientific privateOrderWithTrades'base,
                  counter = qsToScientific privateOrderWithTrades'counter,
                  creationTimestamp = tsmsToUTCTime privateOrderWithTrades'creation_timestamp,
                  expirationTimestamp = tsmsToUTCTime privateOrderWithTrades'expiration_timestamp,
                  feeBase = qsToScientific privateOrderWithTrades'fee_base,
                  feeCounter = qsToScientific privateOrderWithTrades'fee_counter,
                  limitPrice = qsToScientific privateOrderWithTrades'limit_price,
                  limitVolume = qsToScientific privateOrderWithTrades'limit_volume,
                  id = privateOrderWithTrades'order_id,
                  pair = privateOrderWithTrades'pair,
                  state = requestStatusParse privateOrderWithTrades'state,
                  type = orderTypeParse privateOrderWithTrades'type,
                  trades = map aesToRec privateOrderWithTrades'trades} |]

-------------------------------------------- Balance type ------------------------------------------

data Balance_ = Balance_
    { balance'account_id :: AccountID
    , balance'asset :: Asset
    , balance'balance :: QuotedScientific
    , balance'reserved :: QuotedScientific
    , balance'unconfirmed :: QuotedScientific
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Balance_)

instance BitXAesRecordConvert Balance Balance_ where
    aesToRec (Balance_ {..}) =
        [record| {id = balance'account_id,
                  asset = balance'asset,
                  balance = qsToScientific balance'balance,
                  reserved = qsToScientific balance'reserved,
                  unconfirmed = qsToScientific balance'unconfirmed} |]

-------------------------------------------- Balances type -----------------------------------------

data Balances_ = Balances_
    { balances'balance :: [Balance_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Balances_)

instance BitXAesRecordConvert [Balance] Balances_ where
    aesToRec (Balances_ {..}) =
        map aesToRec balances'balance

----------------------------------------- FundingAddress type --------------------------------------

data FundingAddress_ = FundingAddress_
    { fundingAdress'asset :: Asset
    , fundingAdress'address :: Text
    , fundingAdress'total_received :: QuotedScientific
    , fundingAdress'total_unconfirmed :: QuotedScientific
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''FundingAddress_)

instance BitXAesRecordConvert FundingAddress FundingAddress_ where
    aesToRec (FundingAddress_ {..}) =
        [record| {asset = fundingAdress'asset,
                  address = fundingAdress'address,
                  totalReceived = qsToScientific fundingAdress'total_received,
                  totalUnconfirmed = qsToScientific fundingAdress'total_unconfirmed} |]

--------------------------------------------- Asset type -------------------------------------------

instance POSTEncodeable Asset where
    postEncode asset =
        [("asset", showableToBytestring_ asset)]

-------------------------------------- WithdrawalRequest type --------------------------------------

data WithdrawalRequest_ = WithdrawalRequest_
    { withdrawalRequest'status :: RequestStatus_
    , withdrawalRequest'id :: Text
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''WithdrawalRequest_)

instance BitXAesRecordConvert WithdrawalRequest WithdrawalRequest_ where
    aesToRec (WithdrawalRequest_ {..}) =
        [record| {status = requestStatusParse withdrawalRequest'status,
                  id = withdrawalRequest'id} |]

-------------------------------------- WithdrawalRequests type -------------------------------------

data WithdrawalRequests_ = WithdrawalRequests_
    { withdrawalRequests'withdrawals :: [WithdrawalRequest_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''WithdrawalRequests_)

instance BitXAesRecordConvert [WithdrawalRequest] WithdrawalRequests_ where
    aesToRec (WithdrawalRequests_ {..}) =
        map aesToRec withdrawalRequests'withdrawals

----------------------------------------- NewWithdrawal type ---------------------------------------

instance POSTEncodeable NewWithdrawal where
    postEncode nwthd =
        [("type", showableToBytestring_ (view [lens| type |] nwthd)),
         ("amount", showableToBytestring_ (view [lens| amount |] nwthd))]

-------------------------------------- BitcoinSendRequest type -------------------------------------

instance POSTEncodeable BitcoinSendRequest where
    postEncode oreq =
        [("amount", showableToBytestring_ (view [lens| amount |] oreq)),
         ("currency", showableToBytestring_ (view [lens| currency |] oreq)),
         ("address", Txt.encodeUtf8 (view [lens| address |] oreq)),
         ("description", Txt.encodeUtf8 . unjustText $ (view [lens| description |] oreq)),
         ("message", Txt.encodeUtf8 . unjustText $ (view [lens| message |] oreq))]
        where
            unjustText (Just a) = a
            unjustText Nothing  = ""

----------------------------------------- QuoteRequest type ----------------------------------------

instance POSTEncodeable QuoteRequest where
    postEncode oreq =
        [("type", showableToBytestring_ (view [lens| type |] oreq)),
         ("pair", showableToBytestring_ (view [lens| pair |] oreq)),
         ("base_amount", showableToBytestring_ (view [lens| baseAmount |] oreq))]

------------------------------------------ OrderQuote type -----------------------------------------

data OrderQuote_ = OrderQuote_
    { orderQuote'id :: Text
    , orderQuote'type :: QuoteType
    , orderQuote'pair :: CcyPair
    , orderQuote'base_amount :: QuotedScientific
    , orderQuote'counter_amount :: QuotedScientific
    , orderQuote'created_at :: TimestampMS
    , orderQuote'expires_at :: TimestampMS
    , orderQuote'discarded :: Bool
    , orderQuote'exercised :: Bool
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''OrderQuote_)

instance BitXAesRecordConvert OrderQuote OrderQuote_ where
    aesToRec (OrderQuote_ {..}) =
        [record| {id = orderQuote'id,
                  type = orderQuote'type,
                  pair = orderQuote'pair,
                  baseAmount = qsToScientific orderQuote'base_amount,
                  counterAmount = qsToScientific orderQuote'counter_amount,
                  createdAt = tsmsToUTCTime orderQuote'created_at,
                  expiresAt = tsmsToUTCTime orderQuote'expires_at,
                  discarded = orderQuote'discarded,
                  exercised = orderQuote'exercised} |]

-------------------------------------------- BitXAuth type -----------------------------------------

data BitXAuth_ = BitXAuth_
    { bitXAuth'api_key_id :: Text
    , bitXAuth'api_key_secret :: Text
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''BitXAuth_)

instance BitXAesRecordConvert BitXAuth BitXAuth_ where
    aesToRec (BitXAuth_ {..}) =
        [record| {id = bitXAuth'api_key_id,
                  secret = bitXAuth'api_key_secret} |]

------------------------------------------ Transaction type ----------------------------------------

data Transaction_ = Transaction_
    { transaction'row_index :: Int
    , transaction'timestamp :: TimestampMS
    , transaction'balance :: QuotedScientific
    , transaction'available :: QuotedScientific
    , transaction'balance_delta :: QuotedScientific
    , transaction'available_delta :: QuotedScientific
    , transaction'currency :: Asset
    , transaction'description :: Text
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Transaction_)

instance BitXAesRecordConvert Transaction Transaction_ where
    aesToRec (Transaction_ {..}) =
        [record| {rowIndex = transaction'row_index,
                  timestamp = tsmsToUTCTime transaction'timestamp,
                  balance = qsToScientific transaction'balance,
                  available = qsToScientific transaction'available,
                  balanceDelta = qsToScientific transaction'balance_delta,
                  availableDelta = qsToScientific transaction'available_delta,
                  currency = transaction'currency,
                  description = transaction'description} |]

---------------------------------------- Transactions type -----------------------------------------

data Transactions_ = Transactions_
    { transactions'transactions :: [Transaction_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Transactions_)

instance BitXAesRecordConvert [Transaction] Transactions_ where
    aesToRec (Transactions_ {..}) =
        map aesToRec transactions'transactions


data PendingTransactions_ = PendingTransactions_
    { transactions'pending :: [Transaction_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''PendingTransactions_)

instance BitXAesRecordConvert PendingTransactions__ PendingTransactions_ where
    aesToRec (PendingTransactions_ {..}) =
        [record| {transactions = map aesToRec transactions'pending}|]

type PendingTransactions__ =
    [record|
        {transactions :: [Transaction]}|]

pendingTransactionsToTransactions :: PendingTransactions__ -> [Transaction]
pendingTransactionsToTransactions pts = (view [lens| transactions |] pts)

