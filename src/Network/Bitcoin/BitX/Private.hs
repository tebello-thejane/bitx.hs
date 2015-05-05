{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.BitX.Private
  (
  getAllOrders,
  postOrder,
  stopOrder
  ) where

import Network.Bitcoin.BitX.Types
import qualified Network.HTTP.Conduit as NetCon
import Network.HTTP.Conduit (Response(..))
import Control.Exception (try, SomeException)
import qualified Data.Aeson as Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)
import Network (withSocketsDo)
import Network.Bitcoin.BitX.Types.Internal

{- | Returns a list of the most recently placed orders.

This list is truncated after 100 items. -}

getAllOrders :: BitXAuth -> Maybe CcyPair -> IO (Maybe (Either BitXError PrivateOrders))
getAllOrders auth pair = withSocketsDo $ do
    --let request =
    response <- try . NetCon.withManager . NetCon.httpLbs . NetCon.applyBasicAuth "a" "pass"
        . fromJust . NetCon.parseUrl $ "http://google.com/"
        :: IO (Either SomeException (Response BL.ByteString))
    case response of
        Left _  -> return Nothing -- gobble up all exceptions and just return Nothing
        Right k -> do
            let respTE = (Aeson.decode $ NetCon.responseBody k) -- is it a BitX error?
            case respTE of
                Just e  -> return (Just (Left (bitXErrorConverter_ e)))
                Nothing -> do
                    let respTT = (Aeson.decode $ NetCon.responseBody k)
                    case respTT of
                        Just t  -> return (Just (Right (privateOrdersConverter_ t)))
                        Nothing -> return Nothing

   {- case request of
        Nothing
    NetCon.withManager $ \manager -> do
        -- Response _ _ bsrc <- NetCon.http request manager
        return . return $ Nothing-}

{- | Create a new order. -}

postOrder :: BitXAuth -> OrderRequest -> IO OrderID
postOrder = undefined

{- | Request to stop an order. -}

stopOrder :: BitXAuth -> OrderID -> IO StopOrderSuccess
stopOrder = undefined

