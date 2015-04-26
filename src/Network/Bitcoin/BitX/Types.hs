module Network.Bitcoin.BitX.Types
  (
    Ticker,
    CcyPair
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Decimal

data Ticker = Ticker {
  timestamp :: Time,
  bid :: Decimal,
  ask :: Decimal,
  last_trade :: Decimal,
  rolling_24_hour_volume :: Decimal,
  tickerPair :: CcyPair
  } deriving (ToJSON, FromJSON)

data CcyPair = XBTZAR | XBTNAD | ZARXBT | NADXBT
  deriving (Show, Read)

data Orderbook = Orderbook {
  orderTimestamp :: Time,
  bids :: [Bid],
  asks :: [Ask]
} deriving (ToJSON, FromJSON)

data Order = Order {
  orderVolume :: Decimal,
  orderPrice :: Decimal
} deriving (ToJSON, FromJSON)

type Bid = Order
type Ask = Order

data Trade = Trade {
  tradeVolume :: Decimal,
  tradeTimestamp :: Time,
  tradePrice :: Decimal
}

data BitXAuth = BitXAuth {
  bitXID :: String,
  bitXSecret :: String
}

data PrivateOrder = PrivateOrder {
  base :: Decimal,
  counter :: Decimal,
  creationTimestamp :: Time,
  expirationTimestamp :: Time,
  feeBase :: Decimal,
  feeCounter :: Decimal,
  limitPrice :: Decimal,
  limitVolume :: Decimal,
  orderID :: OrderID,
  privateOrderPair :: CcyPair,
  state :: OrderStatus,
  type :: OrderType
}

type OrderID = String

data OrderType = ASK | BID

type OrderStatus = PENDING | COMPLETE

data OrderRequest = OrderRequest {
  requestPair :: CcyPair,
  requestType :: OrderType,
  requestVolume :: Decimal,
  requestPrice :: Decimal,
}

data StopOrderSuccess = StopOrderSuccess



