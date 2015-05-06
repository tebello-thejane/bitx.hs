module Network.Bitcoin.BitX.Public
  (
    getTicker,
    getTickers,
    getOrderBook,
    getTrades
  ) where

import qualified Data.Aeson as Aeson (decode, encode)
import Network.Bitcoin.BitX.Types
import Network.Bitcoin.BitX.Types.Internal
import qualified Network.HTTP.Conduit as NetCon
import qualified Data.ByteString.Lazy as BL
import Control.Exception (try, SomeException)
import Network (withSockets)

simpleBitXGet_ :: BitXRecordConvert rec aes => String -> IO (Maybe (Either BitXError rec))
simpleBitXGet_ verb = withSockets $ do
    resp <- try $ NetCon.simpleHttp ("https://api.mybitx.com/api/1/" ++ verb)
        :: IO (Either SomeException BL.ByteString)
    case resp of
        Left _  -> return Nothing -- gobble up all exceptions and just return Nothing
        Right k -> do
            let respTE = (Aeson.decode $ k) -- is it a BitX error?
            case respTE of
                Just e  -> return (Just (Left (bitXErrorConverter_ e)))
                Nothing -> do
                    let respTT = (Aeson.decode $ k)
                    case respTT of
                        Just t  -> return (Just (Right (aesToRec t)))
                        Nothing -> return Nothing

{- | Returns the latest ticker indicators. -}

getTicker :: CcyPair -> IO (Maybe (Either BitXError Ticker))
getTicker cyp = simpleBitXGet_ $ "ticker?pair=" ++ (show cyp)

{- | Returns the latest ticker indicators from all active BitX exchanges. -}

getTickers :: IO (Maybe (Either BitXError Tickers))
getTickers = simpleBitXGet_ "tickers"

{- | Returns a list of bids and asks in the order book.

Ask orders are sorted by price ascending. Bid orders are sorted by price descending.
Note that multiple orders at the same price are not necessarily conflated. -}

getOrderBook :: CcyPair -> IO (Maybe (Either BitXError Orderbook))
getOrderBook cyp = simpleBitXGet_ $ "orderbook?pair=" ++ (show cyp)

{- | Returns a list of the most recent trades -}

getTrades :: CcyPair -> IO (Maybe (Either BitXError PublicTrades))
getTrades cyp = simpleBitXGet_ $ "trades?pair=" ++ (show cyp)

