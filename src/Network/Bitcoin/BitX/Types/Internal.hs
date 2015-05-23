{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, MultiParamTypeClasses,
    FunctionalDependencies, FlexibleInstances, DataKinds, FlexibleContexts, CPP #-}

{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}

module Network.Bitcoin.BitX.Types.Internal
    (
    BitXAesRecordConvert(..),
    POSTEncodeable(..),
    showableToBytestring_,
    Transaction_(..),
    pendingTransactionsToTransactions,
    PendingTransactions__(..)
    )
where

import Network.Bitcoin.BitX.Types
import Data.Aeson (FromJSON(..), parseJSON, Value(..), toJSON, ToJSON(..))
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


showableToBytestring_ :: (Show a) => a -> ByteString
showableToBytestring_ = Txt.encodeUtf8 . Txt.pack . show

-- | Wrapper around Decimal and FromJSON instance, to facilitate automatic JSON instances

newtype QuotedDecimal = QuotedDecimal Decimal deriving (Read, Show)

instance FromJSON QuotedDecimal where
   parseJSON (String x) = return . QuotedDecimal . read . Txt.unpack $ x
   parseJSON (Number x) = return . QuotedDecimal . read . show $ x
   parseJSON _          = mempty

--instance ToJSON QuotedDecimal where
--    toJSON (QuotedDecimal q) = Number . realToFrac $ q

qdToDecimal :: QuotedDecimal -> Decimal
qdToDecimal (QuotedDecimal dec) = dec

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
orderTypeParse (OrderType_ "BUY") = BID
orderTypeParse (OrderType_ "BID") = BID
orderTypeParse (OrderType_ "ASK") = ASK
orderTypeParse (OrderType_ "SELL") = ASK


newtype RequestStatus_ = RequestStatus_ Text deriving (Read, Show)

instance FromJSON RequestStatus_ where
   parseJSON (String x) = return . RequestStatus_ $ x
   parseJSON _          = mempty

requestStatusParse :: RequestStatus_ -> RequestStatus
requestStatusParse (RequestStatus_ "PENDING") = PENDING
requestStatusParse (RequestStatus_ "COMPLETE") = COMPLETE
requestStatusParse (RequestStatus_ "COMPLETED") = COMPLETE
requestStatusParse (RequestStatus_ "CANCELLED") = CANCELLED

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

--------------------------------------------- Tickers type -----------------------------------------

data Tickers_ = Tickers_
    { tickers'tickers :: [Ticker_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Tickers_)

instance BitXAesRecordConvert [Ticker] Tickers_ where
    aesToRec (Tickers_ tickers''tickers) =
        map aesToRec tickers''tickers

-------------------------------------------- BitXError type ----------------------------------------

data BitXError_= BitXError_
    { bitXError'error :: Text,
      bitXError'error_code :: Text
    } deriving (Show, Eq)

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
    --, publicTrades'currency :: Asset
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''PublicTrades_)

instance BitXAesRecordConvert [Trade] PublicTrades_ where
    aesToRec (PublicTrades_ publicTrades''trades {-publicTrades''currency-}) =
        map aesToRec publicTrades''trades

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
    , privateOrder'state :: RequestStatus_
    , privateOrder'type :: OrderType_
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
                  id = privateOrder''order_id,
                  pair = privateOrder''pair,
                  state = requestStatusParse privateOrder''state,
                  type = orderTypeParse privateOrder''type} |]

------------------------------------------ PrivateOrders type --------------------------------------

data PrivateOrders_ = PrivateOrders_
    {privateOrders'orders :: [PrivateOrder_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''PrivateOrders_)

instance BitXAesRecordConvert [PrivateOrder] PrivateOrders_ where
    aesToRec (PrivateOrders_ privateOrders''orders) =
        map aesToRec privateOrders''orders

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
    aesToRec (OrderIDRec_ orderIDResponse''order_id) =
        orderIDResponse''order_id

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
    aesToRec (RequestSuccess_ requestSuccess''success) =
        requestSuccess''success

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
    , privateOrderWithTrades'state :: RequestStatus_
    , privateOrderWithTrades'type :: OrderType_
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
                  id = privateOrder''order_id,
                  pair = privateOrder''pair,
                  state = requestStatusParse privateOrder''state,
                  type = orderTypeParse privateOrder''type,
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
        [record| {id = balance''account_id,
                  asset = balance''asset,
                  balance = qdToDecimal balance''balance,
                  reserved = qdToDecimal balance''reserved,
                  unconfirmed = qdToDecimal balance''unconfirmed} |]

-------------------------------------------- Balances type -----------------------------------------

data Balances_ = Balances_
    {balances'balance :: [Balance_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Balances_)

instance BitXAesRecordConvert [Balance] Balances_ where
    aesToRec (Balances_ balances''balances) =
        map aesToRec balances''balances

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
        [("asset", showableToBytestring_ asset)]

-------------------------------------- WithdrawalRequest type --------------------------------------

data WithdrawalRequest_ = WithdrawalRequest_
    { withdrawalRequest'status :: RequestStatus_
    , withdrawalRequest'id :: Text
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''WithdrawalRequest_)

instance BitXAesRecordConvert WithdrawalRequest WithdrawalRequest_ where
    aesToRec (WithdrawalRequest_ withdrawalRequest''status withdrawalRequest''id) =
        [record| {status = requestStatusParse withdrawalRequest''status,
                  id = withdrawalRequest''id} |]

-------------------------------------- WithdrawalRequests type -------------------------------------

data WithdrawalRequests_ = WithdrawalRequests_
    { withdrawalRequests'withdrawals :: [WithdrawalRequest_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''WithdrawalRequests_)

instance BitXAesRecordConvert [WithdrawalRequest] WithdrawalRequests_ where
    aesToRec (WithdrawalRequests_ withdrawalRequests''withdrawals) =
        map aesToRec withdrawalRequests''withdrawals

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
    , orderQuote'base_amount :: QuotedDecimal
    , orderQuote'counter_amount :: QuotedDecimal
    , orderQuote'created_at :: TimestampMS
    , orderQuote'expires_at :: TimestampMS
    , orderQuote'discarded :: Bool
    , orderQuote'exercised :: Bool
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''OrderQuote_)

instance BitXAesRecordConvert OrderQuote OrderQuote_ where
    aesToRec (OrderQuote_ orderQuote''id orderQuote''type orderQuote''pair orderQuote''base_amount
        orderQuote''counter_amount orderQuote''created_at orderQuote''expires_at orderQuote''discarded
        orderQuote''exercised) =
        [record| {id = orderQuote''id,
                  type = orderQuote''type,
                  pair = orderQuote''pair,
                  baseAmount = qdToDecimal orderQuote''base_amount,
                  counterAmount = qdToDecimal orderQuote''counter_amount,
                  createdAt = tsmsToUTCTime orderQuote''created_at,
                  expiresAt = tsmsToUTCTime orderQuote''expires_at,
                  discarded = orderQuote''discarded,
                  exercised = orderQuote''exercised} |]

-------------------------------------------- BitXAuth type -----------------------------------------

data BitXAuth_ = BitXAuth_
    { bitXAuth'api_key_id :: Text
    , bitXAuth'api_key_secret :: Text
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''BitXAuth_)

instance BitXAesRecordConvert BitXAuth BitXAuth_ where
    aesToRec (BitXAuth_ bitXAuth''api_key_id bitXAuth''api_key_secret) =
        [record| {id = bitXAuth''api_key_id,
                  secret = bitXAuth''api_key_secret} |]

------------------------------------------ Transaction type ----------------------------------------

data Transaction_ = Transaction_
    { transaction'row_index :: Int
    , transaction'timestamp :: TimestampMS
    , transaction'balance :: QuotedDecimal
    , transaction'available :: QuotedDecimal
    , transaction'balance_delta :: QuotedDecimal
    , transaction'available_delta :: QuotedDecimal
    , transaction'currency :: Asset
    , transaction'description :: Text
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Transaction_)

instance BitXAesRecordConvert Transaction Transaction_ where
    aesToRec (Transaction_ transaction''row_index transaction''timestamp transaction''balance
        transaction''available transaction''balance_delta transaction''available_delta
        transaction''currency transaction''description) =
        [record| {rowIndex = transaction''row_index,
                  timestamp = tsmsToUTCTime transaction''timestamp,
                  balance = qdToDecimal transaction''balance,
                  available = qdToDecimal transaction''available,
                  balanceDelta = qdToDecimal transaction''balance_delta,
                  availableDelta = qdToDecimal transaction''available_delta,
                  currency = transaction''currency,
                  description = transaction''description} |]

---------------------------------------- Transactions type -----------------------------------------

data Transactions_ = Transactions_
    { transactions'transactions :: [Transaction_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Transactions_)

instance BitXAesRecordConvert [Transaction] Transactions_ where
    aesToRec (Transactions_ transactions''transactions) =
        map aesToRec transactions''transactions


data PendingTransactions_ = PendingTransactions_
    { transactions'pending :: [Transaction_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''PendingTransactions_)

instance BitXAesRecordConvert PendingTransactions__ PendingTransactions_ where
    aesToRec (PendingTransactions_ transactions''pending) =
        [record| {transactions = map aesToRec transactions''pending}|]

type PendingTransactions__ =
    [record|
        {transactions :: [Transaction]}|]

pendingTransactionsToTransactions :: PendingTransactions__ -> [Transaction]
pendingTransactionsToTransactions pts = (view [lens| transactions |] pts)
