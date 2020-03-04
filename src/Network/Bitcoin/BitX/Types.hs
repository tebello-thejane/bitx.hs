{-# LANGUAGE DeriveGeneric, QuasiQuotes, OverloadedStrings, DataKinds,
    MultiParamTypeClasses, TemplateHaskell, FunctionalDependencies, FlexibleInstances,
    DeriveDataTypeable, StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Bitcoin.BitX.Types
-- Copyright   :  2016 Tebello Thejane
-- License     :  BSD3
--
-- Maintainer  :  Tebello Thejane <zyxoas+hackage@gmail.com>
-- Stability   :  Experimental
-- Portability :  non-portable (GHC Extensions)
--
-- The types used for the various BitX API calls.
--
-----------------------------------------------------------------------------

module Network.Bitcoin.BitX.Types
  (
    Ticker(..),
    CcyPair(..),
    Orderbook(..),
    Order(..),
    Bid,
    Ask,
    Trade(..),
    BitXAuth(..),
    PrivateOrder(..),
    OrderID,
    OrderType(..),
    RequestStatus(..),
    OrderRequest(..),
    MarketOrderRequest(..),
    RequestSuccess,
    BitXError(..),
    AccountID,
    Asset(..),
    Balance(..),
    FundingAddress(..),
    WithdrawalRequest(..),
    NewWithdrawal(..),
    WithdrawalType(..),
    BitcoinSendRequest(..),
    QuoteRequest(..),
    OrderQuote(..),
    QuoteType(..),
    BitXClientAuth,
    Transaction(..),
    Account(..),
    PrivateTrade(..),
    FeeInfo(..),

-- | Convenient constructors for records which serve as input parameters to functions. These are not
--   completely safe (since you can forget to set a field and the Haskell compiler won't notice),
--   but they are a bit more convenient than dealing with the raw records directly, as long as
--   you're careful.
    mkBitXAuth,
    mkAccount,
    mkBitcoinSendRequest,
    mkOrderRequest,
    mkQuoteRequest,
    mkNewWithdrawal,
    mkMarketOrderRequest,

-- | Lens @Has*@ instances for convenient record accessors and mutators.
--
--   For a broader view of how these function (and why you can generally ignore them) see the
--   documentation for 'lens''s 'Control.Lens.TH.makeFields'.
--
--   Essentially, an instance declaration of the form
--
-- @
-- instance HasFoo MyRecord Int
-- @
--   implies that we can pretend that the data type @MyRecord@ has a field called @Foo@ of type @Int@
--   (although in reality the field would be called @myRecordFoo@ or such), and that there exists a
--   lens called @foo@ which can be used -- among other things -- as a setter and getter on
--   @MyRecord@.
    HasError(..),
    HasErrorCode(..),
    HasTimestamp(..),
    HasBid(..),
    HasAsk(..),
    HasLastTrade(..),
    HasRolling24HourVolume(..),
    HasPair(..),
    HasVolume(..),
    HasPrice(..),
    HasBids(..),
    HasAsks(..),
    HasSecret(..),
    HasId(..),
    HasBase(..),
    HasCounter(..),
    HasCreationTimestamp(..),
    HasExpirationTimestamp(..),
    HasCompletedTimestamp(..),
    HasFeeBase(..),
    HasFeeCounter(..),
    HasLimitPrice(..),
    HasState(..),
    HasOrderType(..),
    HasLimitVolume(..),
    HasRowIndex(..),
    HasBalance(..),
    HasAvailable(..),
    HasBalanceDelta(..),
    HasAvailableDelta(..),
    HasCurrency(..),
    HasDescription(..),
    HasAsset(..),
    HasReserved(..),
    HasUnconfirmed(..),
    HasAddress(..),
    HasTotalReceived(..),
    HasTotalUnconfirmed(..),
    HasAmount(..),
    HasWithdrawalType(..),
    HasMessage(..),
    HasQuoteType(..),
    HasBaseAmount(..),
    HasCounterAmount(..),
    HasCreatedAt(..),
    HasExpiresAt(..),
    HasDiscarded(..),
    HasExercised(..),
    HasName(..),
    HasIsBuy(..),
    HasStatus(..),
    HasBeneficiaryId(..),
    HasOrderId(..),
    HasMakerFee(..),
    HasTakerFee(..),
    HasThirtyDayVolume(..)
  ) where

import Data.Aeson (FromJSON(..))
import Data.Text (Text, pack)
import Data.Time.Clock
import GHC.Generics (Generic)
import Data.Scientific (Scientific)
import Lens.Micro.TH (makeFields)
import Data.String (IsString(..))
import Data.Data (Data, Typeable)
import Control.DeepSeq (NFData)

type OrderID = Text

-- | The type of a placed order.
data OrderType =
    ASK -- ^ A request to sell
    | BID -- ^ A request to buy
    deriving (Show, Generic, Eq, Data, Typeable, Ord)

instance NFData OrderType

-- | The state of a (private) placed request -- either an order or a withdrawal request.
data RequestStatus =
    PENDING -- ^ Not yet completed. An order will stay in 'PENDING' state even as it is partially
    -- filled, and will move to 'COMPLETE' once it has been completely filled.
    | COMPLETE -- ^ Completed.
    | CANCELLED -- ^ Cancelled. Note that an order cannot be in 'CANCELLED' state, since cancelling
    -- an order removes it from the orderbook.
    deriving (Show, Generic, Eq, Data, Typeable, Ord)

instance NFData RequestStatus

type AccountID = Text

-- | A possible error which the BitX API might return,
-- instead of returning the requested data. Note that as yet there is no
-- exhaustive list of error codes available, so comparisons will have to be
-- done via Text comparisons (as opposed to typed pattern matching). Sorry...

data BitXError = BitXError {
    bitXErrorError :: Text,
    bitXErrorErrorCode :: Text
    } deriving (Eq, Generic, Show, Data, Typeable, Ord)

instance NFData BitXError

makeFields ''BitXError

-- | A currency pair
data CcyPair =
    XBTZAR -- ^ Bitcoin vs. ZAR
    | XBTMYR -- ^ Bitcoin vs. Malaysian Ringgit
    | XBTNGN -- ^ Bitcoin vs. Nigerian Naira
    | XBTIDR -- ^ Bitcoin vs. Indonesian Rupiah
    | ETHXBT -- ^ Ether vs. Bitcoin
    | XBTEUR -- ^ Bitcoin vs. Euro
    | ETHZAR -- ^ Ether vs. South African Rand
    | BCHXBT -- ^ Bitcoin vs. Bitcoin Cash
    | ETHMYR -- ^ Ether vs. Malaysian Ringgit
    | XBTZMW -- ^ Bitcoin vs. Zambian Kwacha
    | XBTSGD -- ^ Bitcoin vs. Singapore Dollar
    | XBTUGX -- ^ Bitcoin vs. Ugandan Shilling
    | ETHNGN -- ^ Ether vs. Nigerian Naira
    | XRPZAR -- ^ Ripple vs. South African Rand
    | XRPMYR -- ^ Ripple vs. Malaysian Ringgit
    | XRPXBT -- ^ Ripple vs. Bitcoin
  deriving (Show, Generic, Eq, Data, Typeable, Ord)

instance NFData CcyPair

-- | The state of a single market, identified by the currency pair.
-- As usual, the ask\/sell price is the price of the last filled ask order, and the bid\/buy price is
-- the price of the last filled bid order. Necessarily @bid <= ask.@
data Ticker = Ticker {
    tickerTimestamp :: UTCTime,
    tickerBid :: Maybe Scientific,
    tickerAsk :: Maybe Scientific,
    tickerLastTrade :: Maybe Scientific,
    tickerRolling24HourVolume :: Scientific,
    tickerPair :: CcyPair
    } deriving (Eq, Show, Generic, Data, Typeable, Ord)

instance NFData Ticker

makeFields ''Ticker

-- | A trade-able asset. Essentially, a currency.
data Asset =
    ZAR -- ^ South African Rand
    | XBT -- ^ Bitcoin
    | MYR -- ^ Malaysian Ringgit
    | IDR -- ^ Indonesian Rupiah
    | BCH -- ^ Bitcoin Cash
    | ETH -- ^ Ether
    | ZMW -- ^ Zambian Kwacha
    | NGN -- ^ Nigerian Naira
    | UGX -- ^ Ugandan Shilling
    | SGD -- ^ Singapore Dollar
    | XRP -- ^ Ripple
  deriving (Show, Generic, Eq, Data, Typeable, Ord)

instance NFData Asset

-- | The type of a withdrawal request.
data WithdrawalType =
    ZAR_EFT -- ^ ZAR by Electronic Funds Transfer
    | NAD_EFT -- ^ Namibian Dollar by EFT
    | KES_EFT -- ^ Kenyan Shilling by EFT
    | KES_MPESA -- ^ Kenyan Shilling by Vodafone MPESA
    | MYR_IBG -- ^ Malaysian Ringgit by Interbank GIRO (?)
    | IDR_LLG -- ^ Indonesian Rupiah by Lalu Lintas Giro (??)
    | NGN_EFT -- ^ Nigerian Naira by EFT
    | ZMW_EFT -- ^ Zambian Kwacha by EFT
    | SGD_GIRO -- ^ Singapore Dollar by GIRO / FAST
    | SGD_WIRE -- ^ Singapore Dollar by SWIFT wire
    | EUR_SEPA -- ^ Euro by SEPA
    | UGX_EFT -- ^ Ugandan Shilling by EFT
    deriving (Show, Generic, Eq, Data, Typeable, Ord)

instance NFData WithdrawalType

data QuoteType = BUY | SELL deriving (Show, Generic, Eq, Data, Typeable, Ord)

instance NFData QuoteType

type RequestSuccess = Bool

-- | A single placed order in the orderbook
data Order = Order {
    orderVolume :: Scientific,
    orderPrice :: Scientific
    } deriving (Eq, Show, Generic, Data, Typeable, Ord)

instance NFData Order

makeFields ''Order

-- | Convenient type alias for a bid order
type Bid = Order

-- | Convenient type alias for an ask order
type Ask = Order

-- | The current state of the publically accessible orderbook.
-- Bid orders are requests to buy, ask orders are requests to sell.
data Orderbook = Orderbook {
    orderbookTimestamp :: UTCTime,
    orderbookBids :: [Bid],
    orderbookAsks :: [Ask]
    } deriving (Eq, Show, Generic, Data, Typeable, Ord)

instance NFData Orderbook

makeFields ''Orderbook

data Trade = Trade {
    tradeTimestamp :: UTCTime,
    tradeVolume :: Scientific,
    tradePrice :: Scientific,
    tradeIsBuy :: Bool
    } deriving (Eq, Show, Generic, Data, Typeable, Ord)

instance NFData Trade

makeFields ''Trade

-- | An auth type used by all private API calls, after authorisation.
data BitXAuth = BitXAuth
        {bitXAuthId :: Text,
         bitXAuthSecret :: Text} deriving (Eq, Show, Generic, Data, Typeable, Ord)

instance NFData BitXAuth

-- |@mkBitXAuth = BitXAuth "" ""@
mkBitXAuth :: BitXAuth
mkBitXAuth = BitXAuth "" ""

makeFields ''BitXAuth

type BitXClientAuth = BitXAuth

instance IsString BitXAuth where
  fromString auth =
    BitXAuth (pack $ fst cut) (pack $ tail $ snd cut)
    where
      cut = span (/= ':') auth
-- |
-- >>> :set -XOverloadedStrings
-- >>> "id:secret" :: BitXAuth
-- BitXAuth {bitXAuthId = "id", bitXAuthSecret = "secret"}
-- >>> "id:se:cret" :: BitXAuth
-- BitXAuth {bitXAuthId = "id", bitXAuthSecret = "se:cret"}


-- | A recently placed (private) order, containing a lot more information than is available on the
-- public order book.
data PrivateOrder = PrivateOrder
        {privateOrderBase :: Scientific,
         privateOrderCounter :: Scientific,
         privateOrderCreationTimestamp :: UTCTime,
         privateOrderExpirationTimestamp :: UTCTime,
         privateOrderCompletedTimestamp :: UTCTime,
         privateOrderFeeBase :: Scientific,
         privateOrderFeeCounter :: Scientific,
         privateOrderLimitPrice :: Scientific,
         privateOrderLimitVolume :: Scientific,
         privateOrderId :: OrderID,
         privateOrderPair :: CcyPair,
         privateOrderState :: RequestStatus,
         privateOrderOrderType :: OrderType } deriving (Eq, Show, Generic, Data, Typeable)

instance NFData PrivateOrder

makeFields ''PrivateOrder

-- | A transaction on a private user account.
data Transaction = Transaction
        {transactionRowIndex :: Int,
         transactionTimestamp :: UTCTime,
         transactionBalance :: Scientific,
         transactionAvailable :: Scientific,
         transactionBalanceDelta :: Scientific,
         transactionAvailableDelta :: Scientific,
         transactionCurrency :: Asset,
         transactionDescription :: Text} deriving (Eq, Show, Generic, Data, Typeable, Ord)

instance NFData Transaction

makeFields ''Transaction

-- | A request to place an order.
data OrderRequest = OrderRequest
        {orderRequestPair :: CcyPair,
         orderRequestOrderType :: OrderType,
         orderRequestVolume :: Scientific,
         orderRequestPrice :: Scientific } deriving (Eq, Show, Generic, Data, Typeable, Ord)

instance NFData OrderRequest

makeFields ''OrderRequest

-- |@mkOrderRequest = OrderRequest XBTZAR BID 0 0@
mkOrderRequest :: OrderRequest
mkOrderRequest = OrderRequest XBTZAR BID 0 0

data MarketOrderRequest = MarketOrderRequest
        {marketOrderRequestPair :: CcyPair,
         marketOrderRequestOrderType :: OrderType,
         marketOrderRequestVolume :: Scientific } deriving (Eq, Show, Generic, Data, Typeable, Ord)

instance NFData MarketOrderRequest

makeFields ''MarketOrderRequest

-- |@mkMarketOrderRequest = MarketOrderRequest XBTZAR BID 0@
mkMarketOrderRequest :: MarketOrderRequest
mkMarketOrderRequest = MarketOrderRequest XBTZAR BID 0

-- | The current balance of a private account.
data Balance = Balance
        {balanceId :: AccountID,
         balanceAsset :: Asset,
         balanceBalance :: Scientific,
         balanceReserved :: Scientific,
         balanceUnconfirmed :: Scientific } deriving (Eq, Show, Generic, Data, Typeable, Ord)

instance NFData Balance

makeFields ''Balance

-- | A registered address for an acocunt.
data FundingAddress = FundingAddress
        {fundingAddressAsset :: Asset,
         fundingAddressAddress :: Text,
         fundingAddressTotalReceived :: Scientific,
         fundingAddressTotalUnconfirmed :: Scientific} deriving (Eq, Show, Generic, Data, Typeable, Ord)

instance NFData FundingAddress

makeFields ''FundingAddress

-- | The state of a request to withdraw from an account.
data WithdrawalRequest = WithdrawalRequest
        {withdrawalRequestStatus :: RequestStatus,
         withdrawalRequestId :: Text } deriving (Eq, Show, Generic, Data, Typeable, Ord)

instance NFData WithdrawalRequest

makeFields ''WithdrawalRequest

-- | A request to withdraw from an account.
data NewWithdrawal = NewWithdrawal
        {newWithdrawalWithdrawalType :: WithdrawalType,
         newWithdrawalAmount :: Scientific,
         newWithdrawalBeneficiaryId :: Maybe Text} deriving (Eq, Show, Generic, Data, Typeable, Ord)

instance NFData NewWithdrawal

makeFields ''NewWithdrawal

-- |@mkNewWithdrawal = NewWithdrawal ZAR_EFT 0@
mkNewWithdrawal :: NewWithdrawal
mkNewWithdrawal = NewWithdrawal ZAR_EFT 0 Nothing

-- | A request to send bitcoin to a bitcoin address or email address.
data BitcoinSendRequest = BitcoinSendRequest
        {bitcoinSendRequestAmount :: Scientific,
         bitcoinSendRequestCurrency :: Asset,
         bitcoinSendRequestAddress :: Text,
         bitcoinSendRequestDescription :: Maybe Text,
         bitcoinSendRequestMessage :: Maybe Text} deriving (Eq, Show, Generic, Data, Typeable, Ord)

instance NFData BitcoinSendRequest

makeFields ''BitcoinSendRequest

-- |@mkBitcoinSendRequest = BitcoinSendRequest 0 ZAR "" Nothing Nothing@
mkBitcoinSendRequest :: BitcoinSendRequest
mkBitcoinSendRequest = BitcoinSendRequest 0 ZAR "" Nothing Nothing

-- | A request to lock in a quote.
data QuoteRequest = QuoteRequest
        {quoteRequestQuoteType :: QuoteType,
         quoteRequestPair :: CcyPair,
         quoteRequestBaseAmount :: Scientific} deriving (Eq, Show, Generic, Data, Typeable, Ord)

instance NFData QuoteRequest

makeFields ''QuoteRequest

-- |@mkQuoteRequest = QuoteRequest BUY XBTZAR 0@
mkQuoteRequest :: QuoteRequest
mkQuoteRequest = QuoteRequest BUY XBTZAR 0

-- | A temporarily locked-in quote.
data OrderQuote = OrderQuote
        {orderQuoteId :: Text,
         orderQuoteQuoteType :: QuoteType,
         orderQuotePair :: CcyPair,
         orderQuoteBaseAmount :: Scientific,
         orderQuoteCounterAmount :: Scientific,
         orderQuoteCreatedAt :: UTCTime,
         orderQuoteExpiresAt :: UTCTime,
         orderQuoteDiscarded :: Bool,
         orderQuoteExercised :: Bool} deriving (Eq, Show, Generic, Data, Typeable, Ord)

instance NFData OrderQuote

makeFields ''OrderQuote

-- | A registered account.
data Account = Account
        {accountId :: Text,
         accountName :: Text,
         accountCurrency :: Asset} deriving (Eq, Show, Generic, Data, Typeable, Ord)

instance NFData Account

makeFields ''Account

-- |@mkAccount = Account "" "" ZAR@
mkAccount :: Account
mkAccount = Account "" "" ZAR

-- | A private trade, containing a lot more information than is avaiable when inspecting trades
-- via the public API.
data PrivateTrade = PrivateTrade {
    privateTradeBase :: Scientific,
    privateTradeCounter :: Scientific,
    privateTradeFeeBase :: Scientific,
    privateTradeFeeCounter :: Scientific,
    privateTradeIsBuy :: Bool,
    privateTradeOrderId :: Text,
    privateTradePair :: CcyPair,
    privateTradePrice :: Scientific,
    privateTradeTimestamp :: UTCTime,
    privateTradeOrderType :: OrderType,
    privateTradeVolume :: Scientific
    } deriving (Eq, Show, Generic, Data, Typeable, Ord)

instance NFData PrivateTrade

makeFields ''PrivateTrade

-- | Fee information
data FeeInfo = FeeInfo {
    feeInfoMakerFee :: Scientific,
    feeInfoTakerFee :: Scientific,
    feeInfoThirtyDayVolume :: Scientific
  } deriving (Eq, Show, Generic, Data, Typeable, Ord)

makeFields ''FeeInfo

instance FromJSON CcyPair

instance FromJSON Asset

instance FromJSON OrderType

instance FromJSON WithdrawalType

instance FromJSON QuoteType
