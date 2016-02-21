{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, TypeFamilies, FlexibleContexts,
    FlexibleInstances, DataKinds, CPP, RecordWildCards, GeneralizedNewtypeDeriving #-}

module Network.Bitcoin.BitX.Types.Internal
    (
    BitXAesRecordConvert(..),
    POSTEncodeable(..),
    Transaction_(..),
    pendingTransactionsToTransactions,
    timeToTimestamp
    )
where

import Network.Bitcoin.BitX.Types.Internal.Decimal
import qualified Network.Bitcoin.BitX.Types as Types
import Data.Aeson (FromJSON(..), parseJSON, Value(..))
import qualified Data.Aeson.TH as AesTH
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as Txt
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Lens.Micro ((^.))
#if __GLASGOW_HASKELL__ >= 710
-- base 4.8+ (GHC 7.10+) re-exports Monoid and its functions/constants
#else
import Data.Monoid (mempty)
#endif
import Data.Scientific (Scientific)
import Data.ByteString (ByteString)
import Data.List.Split (splitOn)
import qualified Data.ByteString.Char8 as BS8 (pack)
#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce
#endif
import Test.QuickCheck

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

newtype UnixStampMS = UnixStampMS {unUnixStampMS_ :: Integer} deriving (Eq, Ord, Show, Num, Integral, Real, Enum)

instance Arbitrary UnixStampMS where
    arbitrary = do
        k <- choose (1225497600000, 32782406400000) -- 1 Nov 2008 00:00:00 to 1 Nov 3008 00:00:00
        return $ UnixStampMS k

timestampParse_ :: Integer -> UTCTime
timestampParse_ = posixSecondsToUTCTime
    . realToFrac
    . ( / 1000)
    . (fromIntegral :: Integer -> Scientific)

timeToTimestamp :: UTCTime -> Integer
timeToTimestamp = truncate . (* 1000). utcTimeToPOSIXSeconds

-- $setup
-- >>> import Test.QuickCheck

-- |
-- prop> \ n -> (timeToTimestamp $ timestampParse_ $ unUnixStampMS_ n) == unUnixStampMS_ n
--

class FromJSON (Aes recd) => BitXAesRecordConvert recd where
    type Aes recd
    aesToRec :: Aes recd -> recd

class POSTEncodeable recd where
    postEncode :: recd -> [(ByteString, ByteString)]


showableToBytestring_ :: Show a => a -> ByteString
showableToBytestring_ = Txt.encodeUtf8 . Txt.pack . show

-- | Wrappers around Scientific and Int, and FromJSON instance, to facilitate automatic JSON instances

newtype QuotedScientific = QuotedScientific Scientific deriving (Read, Show)
newtype QuotedInt = QuotedInt Int deriving (Read, Show)

instance FromJSON QuotedScientific where
   parseJSON (String x) = return . QuotedScientific . read . Txt.unpack $ x
   parseJSON (Number x) = return . QuotedScientific . read . show $ x
   parseJSON _          = mempty

instance FromJSON QuotedInt where
   parseJSON (String x) = return . QuotedInt . (truncate :: Scientific -> Int) . read . Txt.unpack $ x
   parseJSON (Number x) = return . QuotedInt . (truncate :: Scientific -> Int) . read . show $ x
   parseJSON _          = mempty

qsToScientific :: QuotedScientific -> Scientific
#if __GLASGOW_HASKELL__ >= 708
qsToScientific = coerce
{-# INLINE qsToScientific #-}
#else
qsToScientific (QuotedScientific sci) = sci
#endif

qiToInt :: QuotedInt -> Int
#if __GLASGOW_HASKELL__ >= 708
qiToInt = coerce
{-# INLINE qiToInt #-}
#else
qiToInt (QuotedInt i) = i
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

orderTypeParse :: OrderType_ -> Types.OrderType
orderTypeParse (OrderType_ "BUY")  = Types.BID
orderTypeParse (OrderType_ "BID")  = Types.BID
orderTypeParse (OrderType_ "ASK")  = Types.ASK
orderTypeParse (OrderType_ "SELL") = Types.ASK
orderTypeParse (OrderType_    x  ) =
    error $ "Yet another surprise from the BitX API: unexpected OrderType " ++ Txt.unpack x


newtype RequestStatus_ = RequestStatus_ Text deriving (Read, Show)

instance FromJSON RequestStatus_ where
   parseJSON (String x) = return . RequestStatus_ $ x
   parseJSON _          = mempty

requestStatusParse :: RequestStatus_ -> Types.RequestStatus
requestStatusParse (RequestStatus_ "PENDING")   = Types.PENDING
requestStatusParse (RequestStatus_ "COMPLETE")  = Types.COMPLETE
requestStatusParse (RequestStatus_ "COMPLETED") = Types.COMPLETE
requestStatusParse (RequestStatus_ "CANCELLED") = Types.CANCELLED
requestStatusParse (RequestStatus_      x     ) =
    error $ "Yet another surprise from the BitX API: unexpected RequestStatus " ++ Txt.unpack x

-------------------------------------------- Ticker type -------------------------------------------

data Ticker_ = Ticker_
    { ticker'timestamp :: TimestampMS
    , ticker'bid :: QuotedInt
    , ticker'ask :: QuotedInt
    , ticker'last_trade :: QuotedInt
    , ticker'rolling_24_hour_volume :: QuotedScientific
    , ticker'pair :: Types.CcyPair
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Ticker_)

instance BitXAesRecordConvert Types.Ticker where
    type Aes Types.Ticker = Ticker_
    aesToRec Ticker_ {..} =
        Types.Ticker {tickerTimestamp = tsmsToUTCTime ticker'timestamp,
                  tickerBid = qiToInt ticker'bid,
                  tickerAsk = qiToInt ticker'ask,
                  tickerLastTrade = qiToInt ticker'last_trade,
                  tickerRolling24HourVolume = qsToScientific ticker'rolling_24_hour_volume,
                  tickerPair = ticker'pair}

--------------------------------------------- Tickers type -----------------------------------------

data Tickers_ = Tickers_
    { tickers'tickers :: [Ticker_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Tickers_)

instance BitXAesRecordConvert [Types.Ticker] where
    type Aes [Types.Ticker] = Tickers_
    aesToRec Tickers_ {..} =
        map aesToRec tickers'tickers

-------------------------------------------- BitXError type ----------------------------------------

data BitXError_= BitXError_
    { bitXError'error :: Text,
      bitXError'error_code :: Text
    } deriving (Show, Eq)

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"} ''BitXError_)

instance BitXAesRecordConvert Types.BitXError where
    type Aes Types.BitXError = BitXError_
    aesToRec BitXError_ {..} =
        Types.BitXError { bitXErrorError = bitXError'error, bitXErrorErrorCode = bitXError'error_code}

-------------------------------------------- Order type --------------------------------------------

data Order_ = Order_
    { order'volume :: QuotedScientific,
      order'price :: QuotedInt
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"} ''Order_)

instance BitXAesRecordConvert Types.Order where
    type Aes Types.Order = Order_
    aesToRec Order_ {..} =
        Types.Order {orderVolume = qsToScientific order'volume,
              orderPrice = qiToInt order'price}

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

instance BitXAesRecordConvert Types.Orderbook where
    type Aes Types.Orderbook = Orderbook_
    aesToRec Orderbook_ {..} =
        Types.Orderbook {orderbookTimestamp = tsmsToUTCTime orderbook'timestamp,
                  orderbookBids = map aesToRec orderbook'bids,
                  orderbookAsks = map aesToRec orderbook'asks}

-------------------------------------------- Trade type --------------------------------------------

data Trade_ = Trade_
    { trade'volume :: QuotedScientific
    , trade'timestamp :: TimestampMS
    , trade'price :: QuotedInt
    , trade'is_buy :: Bool
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Trade_)

instance BitXAesRecordConvert Types.Trade where
    type Aes Types.Trade = Trade_
    aesToRec Trade_ {..} =
        Types.Trade { tradeTimestamp = tsmsToUTCTime trade'timestamp,
            tradeVolume = qsToScientific trade'volume,
            tradePrice = qiToInt trade'price,
            tradeIsBuy = trade'is_buy}

----------------------------------------- PublicTrades type ----------------------------------------

data PublicTrades_ = PublicTrades_
    { publicTrades'trades :: [Trade_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''PublicTrades_)

instance BitXAesRecordConvert [Types.Trade] where
    type Aes [Types.Trade] = PublicTrades_
    aesToRec PublicTrades_ {..} =
        map aesToRec publicTrades'trades

------------------------------------------ PrivateOrder type ---------------------------------------

data PrivateOrder_ = PrivateOrder_
    { privateOrder'base :: QuotedScientific
    , privateOrder'counter :: QuotedScientific
    , privateOrder'creation_timestamp :: TimestampMS
    , privateOrder'expiration_timestamp :: TimestampMS
    , privateOrder'fee_base :: QuotedScientific
    , privateOrder'fee_counter :: QuotedScientific
    , privateOrder'limit_price :: QuotedInt
    , privateOrder'limit_volume :: QuotedScientific
    , privateOrder'order_id :: Types.OrderID
    , privateOrder'pair :: Types.CcyPair
    , privateOrder'state :: RequestStatus_
    , privateOrder'type :: OrderType_
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''PrivateOrder_)

instance BitXAesRecordConvert Types.PrivateOrder where
    type Aes Types.PrivateOrder = PrivateOrder_
    aesToRec PrivateOrder_ {..} =
        Types.PrivateOrder {privateOrderBase = qsToScientific privateOrder'base,
                  privateOrderCounter = qsToScientific privateOrder'counter,
                  privateOrderCreationTimestamp = tsmsToUTCTime privateOrder'creation_timestamp,
                  privateOrderExpirationTimestamp = tsmsToUTCTime privateOrder'expiration_timestamp,
                  privateOrderFeeBase = qsToScientific privateOrder'fee_base,
                  privateOrderFeeCounter = qsToScientific privateOrder'fee_counter,
                  privateOrderLimitPrice = qiToInt privateOrder'limit_price,
                  privateOrderLimitVolume = qsToScientific privateOrder'limit_volume,
                  privateOrderId = privateOrder'order_id,
                  privateOrderPair = privateOrder'pair,
                  privateOrderState = requestStatusParse privateOrder'state,
                  privateOrderOrderType = orderTypeParse privateOrder'type}

------------------------------------------ PrivateOrders type --------------------------------------

data PrivateOrders_ = PrivateOrders_
    {privateOrders'orders :: [PrivateOrder_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''PrivateOrders_)

instance BitXAesRecordConvert [Types.PrivateOrder] where
    type Aes [Types.PrivateOrder] = PrivateOrders_
    aesToRec PrivateOrders_ {..} =
        map aesToRec privateOrders'orders

------------------------------------------ OrderRequest type ---------------------------------------

instance POSTEncodeable Types.OrderRequest where
    postEncode oreq =
        [("pair", showableToBytestring_ (oreq ^. Types.pair)),
         ("type", showableToBytestring_ (oreq ^. Types.orderType)),
         ("volume", realToDecimalByteString_ (oreq ^. Types.volume)),
         ("price", BS8.pack . show $ (oreq ^. Types.price))]

-------------------------------------------- OrderIDRec type ---------------------------------------

data OrderIDRec_ = OrderIDRec_
    { orderIDResponse'order_id :: Types.OrderID
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''OrderIDRec_)

instance BitXAesRecordConvert Types.OrderID where
    type Aes Types.OrderID = OrderIDRec_
    aesToRec OrderIDRec_ {..} =
        orderIDResponse'order_id

instance POSTEncodeable Types.OrderID where
    postEncode oid =
        [("order_id", Txt.encodeUtf8 oid)]

----------------------------------------- RequestSuccess type --------------------------------------

data RequestSuccess_ = RequestSuccess_
    { requestSuccess'success :: Bool
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''RequestSuccess_)

instance BitXAesRecordConvert Types.RequestSuccess where
    type Aes Types.RequestSuccess = RequestSuccess_
    aesToRec RequestSuccess_ {..} =
        requestSuccess'success

------------------------------------- PrivateOrderWithTrades type ----------------------------------

data PrivateOrderWithTrades_ = PrivateOrderWithTrades_
    { privateOrderWithTrades'base :: QuotedScientific
    , privateOrderWithTrades'counter :: QuotedScientific
    , privateOrderWithTrades'creation_timestamp :: TimestampMS
    , privateOrderWithTrades'expiration_timestamp :: TimestampMS
    , privateOrderWithTrades'fee_base :: QuotedScientific
    , privateOrderWithTrades'fee_counter :: QuotedScientific
    , privateOrderWithTrades'limit_price :: QuotedInt
    , privateOrderWithTrades'limit_volume :: QuotedScientific
    , privateOrderWithTrades'order_id :: Types.OrderID
    , privateOrderWithTrades'pair :: Types.CcyPair
    , privateOrderWithTrades'state :: RequestStatus_
    , privateOrderWithTrades'type :: OrderType_
    , privateOrderWithTrades'trades :: [Trade_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''PrivateOrderWithTrades_)

instance BitXAesRecordConvert Types.PrivateOrderWithTrades where
    type Aes Types.PrivateOrderWithTrades = PrivateOrderWithTrades_
    aesToRec PrivateOrderWithTrades_ {..} =
        Types.PrivateOrderWithTrades {privateOrderWithTradesBase = qsToScientific privateOrderWithTrades'base,
                  privateOrderWithTradesCounter = qsToScientific privateOrderWithTrades'counter,
                  privateOrderWithTradesCreationTimestamp = tsmsToUTCTime privateOrderWithTrades'creation_timestamp,
                  privateOrderWithTradesExpirationTimestamp = tsmsToUTCTime privateOrderWithTrades'expiration_timestamp,
                  privateOrderWithTradesFeeBase = qsToScientific privateOrderWithTrades'fee_base,
                  privateOrderWithTradesFeeCounter = qsToScientific privateOrderWithTrades'fee_counter,
                  privateOrderWithTradesLimitPrice = qiToInt privateOrderWithTrades'limit_price,
                  privateOrderWithTradesLimitVolume = qsToScientific privateOrderWithTrades'limit_volume,
                  privateOrderWithTradesId = privateOrderWithTrades'order_id,
                  privateOrderWithTradesPair = privateOrderWithTrades'pair,
                  privateOrderWithTradesState = requestStatusParse privateOrderWithTrades'state,
                  privateOrderWithTradesOrderType = orderTypeParse privateOrderWithTrades'type,
                  privateOrderWithTradesTrades = map aesToRec privateOrderWithTrades'trades}

-------------------------------------------- Balance type ------------------------------------------

data Balance_ = Balance_
    { balance'account_id :: Types.AccountID
    , balance'asset :: Types.Asset
    , balance'balance :: QuotedScientific
    , balance'reserved :: QuotedScientific
    , balance'unconfirmed :: QuotedScientific
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Balance_)

instance BitXAesRecordConvert Types.Balance where
    type Aes Types.Balance = Balance_
    aesToRec Balance_ {..} =
        Types.Balance {balanceId = balance'account_id,
                  balanceAsset = balance'asset,
                  balanceBalance = qsToScientific balance'balance,
                  balanceReserved = qsToScientific balance'reserved,
                  balanceUnconfirmed = qsToScientific balance'unconfirmed}

-------------------------------------------- Balances type -----------------------------------------

data Balances_ = Balances_
    { balances'balance :: [Balance_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Balances_)

instance BitXAesRecordConvert [Types.Balance] where
    type Aes [Types.Balance] = Balances_
    aesToRec Balances_ {..} =
        map aesToRec balances'balance

----------------------------------------- FundingAddress type --------------------------------------

data FundingAddress_ = FundingAddress_
    { fundingAdress'asset :: Types.Asset
    , fundingAdress'address :: Text
    , fundingAdress'total_received :: QuotedScientific
    , fundingAdress'total_unconfirmed :: QuotedScientific
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''FundingAddress_)

instance BitXAesRecordConvert Types.FundingAddress where
    type Aes Types.FundingAddress = FundingAddress_
    aesToRec FundingAddress_ {..} =
        Types.FundingAddress {fundingAddressAsset = fundingAdress'asset,
                  fundingAddressAddress = fundingAdress'address,
                  fundingAddressTotalReceived = qsToScientific fundingAdress'total_received,
                  fundingAddressTotalUnconfirmed = qsToScientific fundingAdress'total_unconfirmed}

--------------------------------------------- Asset type -------------------------------------------

instance POSTEncodeable Types.Asset where
    postEncode asset =
        [("asset", showableToBytestring_ asset)]

-------------------------------------- WithdrawalRequest type --------------------------------------

data WithdrawalRequest_ = WithdrawalRequest_
    { withdrawalRequest'status :: RequestStatus_
    , withdrawalRequest'id :: Text
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''WithdrawalRequest_)

instance BitXAesRecordConvert Types.WithdrawalRequest where
    type Aes Types.WithdrawalRequest = WithdrawalRequest_
    aesToRec WithdrawalRequest_ {..} =
        Types.WithdrawalRequest {withdrawalRequestStatus = requestStatusParse withdrawalRequest'status,
                  withdrawalRequestId = withdrawalRequest'id}

-------------------------------------- WithdrawalRequests type -------------------------------------

data WithdrawalRequests_ = WithdrawalRequests_
    { withdrawalRequests'withdrawals :: [WithdrawalRequest_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''WithdrawalRequests_)

instance BitXAesRecordConvert [Types.WithdrawalRequest] where
    type Aes [Types.WithdrawalRequest] = WithdrawalRequests_
    aesToRec WithdrawalRequests_ {..} =
        map aesToRec withdrawalRequests'withdrawals

----------------------------------------- NewWithdrawal type ---------------------------------------

instance POSTEncodeable Types.NewWithdrawal where
    postEncode nwthd =
        [("type", showableToBytestring_ (nwthd ^. Types.withdrawalType)),
         ("amount", realToDecimalByteString_ (nwthd ^. Types.amount))]

-------------------------------------- BitcoinSendRequest type -------------------------------------

instance POSTEncodeable Types.BitcoinSendRequest where
    postEncode oreq =
        [("amount", realToDecimalByteString_ (oreq ^. Types.amount)),
         ("currency", showableToBytestring_ (oreq ^. Types.currency)),
         ("address", Txt.encodeUtf8 (oreq ^. Types.address)),
         ("description", Txt.encodeUtf8 . unjustText $ (oreq ^. Types.description)),
         ("message", Txt.encodeUtf8 . unjustText $ (oreq ^. Types.message))]
        where
            unjustText (Just a) = a
            unjustText Nothing  = ""

----------------------------------------- QuoteRequest type ----------------------------------------

instance POSTEncodeable Types.QuoteRequest where
    postEncode oreq =
        [("type", showableToBytestring_ (oreq ^. Types.quoteType)),
         ("pair", showableToBytestring_ (oreq ^. Types.pair)),
         ("base_amount", realToDecimalByteString_ (oreq ^. Types.baseAmount))]

------------------------------------------ OrderQuote type -----------------------------------------

data OrderQuote_ = OrderQuote_
    { orderQuote'id :: Text
    , orderQuote'type :: Types.QuoteType
    , orderQuote'pair :: Types.CcyPair
    , orderQuote'base_amount :: QuotedScientific
    , orderQuote'counter_amount :: QuotedScientific
    , orderQuote'created_at :: TimestampMS
    , orderQuote'expires_at :: TimestampMS
    , orderQuote'discarded :: Bool
    , orderQuote'exercised :: Bool
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''OrderQuote_)

instance BitXAesRecordConvert Types.OrderQuote where
    type Aes Types.OrderQuote = OrderQuote_
    aesToRec OrderQuote_ {..} =
        Types.OrderQuote {orderQuoteId = orderQuote'id,
                  orderQuoteQuoteType = orderQuote'type,
                  orderQuotePair = orderQuote'pair,
                  orderQuoteBaseAmount = qsToScientific orderQuote'base_amount,
                  orderQuoteCounterAmount = qsToScientific orderQuote'counter_amount,
                  orderQuoteCreatedAt = tsmsToUTCTime orderQuote'created_at,
                  orderQuoteExpiresAt = tsmsToUTCTime orderQuote'expires_at,
                  orderQuoteDiscarded = orderQuote'discarded,
                  orderQuoteExercised = orderQuote'exercised}

-------------------------------------------- BitXAuth type -----------------------------------------

data BitXAuth_ = BitXAuth_
    { bitXAuth'api_key_id :: Text
    , bitXAuth'api_key_secret :: Text
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''BitXAuth_)

instance BitXAesRecordConvert Types.BitXAuth where
    type Aes Types.BitXAuth = BitXAuth_
    aesToRec BitXAuth_ {..} =
        Types.BitXAuth {bitXAuthId = bitXAuth'api_key_id,
                  bitXAuthSecret = bitXAuth'api_key_secret}

------------------------------------------ Transaction type ----------------------------------------

data Transaction_ = Transaction_
    { transaction'row_index :: Int
    , transaction'timestamp :: TimestampMS
    , transaction'balance :: QuotedScientific
    , transaction'available :: QuotedScientific
    , transaction'balance_delta :: QuotedScientific
    , transaction'available_delta :: QuotedScientific
    , transaction'currency :: Types.Asset
    , transaction'description :: Text
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Transaction_)

instance BitXAesRecordConvert Types.Transaction where
    type Aes Types.Transaction = Transaction_
    aesToRec Transaction_ {..} =
        Types.Transaction {transactionRowIndex = transaction'row_index,
                  transactionTimestamp = tsmsToUTCTime transaction'timestamp,
                  transactionBalance = qsToScientific transaction'balance,
                  transactionAvailable = qsToScientific transaction'available,
                  transactionBalanceDelta = qsToScientific transaction'balance_delta,
                  transactionAvailableDelta = qsToScientific transaction'available_delta,
                  transactionCurrency = transaction'currency,
                  transactionDescription = transaction'description}

---------------------------------------- Transactions type -----------------------------------------

data Transactions_ = Transactions_
    { transactions'transactions :: [Transaction_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Transactions_)

instance BitXAesRecordConvert [Types.Transaction] where
    type Aes [Types.Transaction] = Transactions_
    aesToRec Transactions_ {..} =
        map aesToRec transactions'transactions


data PendingTransactions_ = PendingTransactions_
    { transactions'pending :: [Transaction_]
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''PendingTransactions_)

instance BitXAesRecordConvert PendingTransactions__ where
    type Aes PendingTransactions__ = PendingTransactions_
    aesToRec PendingTransactions_ {..} =
        PendingTransactions__ {pendingTransactions__transactions = map aesToRec transactions'pending}

data PendingTransactions__ = PendingTransactions__
        {pendingTransactions__transactions :: [Types.Transaction]}

pendingTransactionsToTransactions :: PendingTransactions__ -> [Types.Transaction]
pendingTransactionsToTransactions (PendingTransactions__ tx) = tx

-------------------------------------------- Account type ------------------------------------------

data Account_ = Account_
    { account'id :: Text
    , account'name :: Text
    , account'currency :: Types.Asset
    }

$(AesTH.deriveFromJSON AesTH.defaultOptions{AesTH.fieldLabelModifier = last . splitOn "'"}
    ''Account_)

instance BitXAesRecordConvert Types.Account where
    type Aes Types.Account = Account_
    aesToRec Account_ {..} =
        Types.Account {accountId = account'id,
                  accountName = account'name,
                  accountCurrency = account'currency}

instance POSTEncodeable Types.Account where
    postEncode acc =
        [("name", showableToBytestring_ (acc ^. Types.name)),
         ("currency", showableToBytestring_ (acc ^. Types.currency))]

------------------------------------- MarketOrderRequest type ------------------------------------

instance POSTEncodeable Types.MarketOrderRequest where
    postEncode moreq =
        [("type", if (moreq ^. Types.orderType) == Types.BID then "BUY" else "SELL"),
         ("pair", showableToBytestring_ (moreq ^. Types.pair)),
         (if (moreq ^. Types.orderType) == Types.BID then "counter_volume" else "base_volume", realToDecimalByteString_ (moreq ^. Types.volume))]
