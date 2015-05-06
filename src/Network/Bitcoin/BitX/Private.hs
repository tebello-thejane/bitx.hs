{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module Network.Bitcoin.BitX.Private
  (
  getAllOrders,
  postOrder,
  stopOrder,
  getPendingOrders
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
import Record (lens)
import Record.Lens (view)
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as Txt

simpleBitXGetAuth_ :: BitXAesRecordConvert rec aes => BitXAuth -> String -> IO (Maybe (Either BitXError rec))
simpleBitXGetAuth_ auth url = withSocketsDo $ do
    response <- try . NetCon.withManager . NetCon.httpLbs . NetCon.applyBasicAuth
          userID
          userSecret
        . fromJust . NetCon.parseUrl $ url
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
                        Just t  -> return (Just (Right (aesToRec t)))
                        Nothing -> return Nothing
    where
        userID = Txt.encodeUtf8 $ (view [lens| id |] auth)
        userSecret = Txt.encodeUtf8 $ (view [lens| secret |] auth)

{- | Returns a list of the most recently placed orders.

If the second parameter is @Nothing@ then this will return orders for all markets, whereas if it is
@Just cpy@ for some @CcyPair cpy@ then the results will be specific to that market.

This list is truncated after 100 items. -}

getAllOrders :: BitXAuth -> Maybe CcyPair -> IO (Maybe (Either BitXError PrivateOrders))
getAllOrders auth pair = simpleBitXGetAuth_ auth url
    where
        url = case pair of
            Nothing  -> "https://api.mybitx.com/api/1/listorders"
            Just st  -> "https://api.mybitx.com/api/1/listorders?pair=" ++ show st

{- | Returns a list of the most recently placed orders which are still in 'PENDING' state.

If the second parameter is @Nothing@ then this will return orders for all markets, whereas if it is
@Just cpy@ for some @CcyPair cpy@ then the results will be specific to that market.

This list is truncated after 100 items. -}

getPendingOrders :: BitXAuth -> Maybe CcyPair -> IO (Maybe (Either BitXError PrivateOrders))
getPendingOrders auth pair = simpleBitXGetAuth_ auth url
    where
        url = case pair of
            Nothing  -> "https://api.mybitx.com/api/1/listorders?state=PENDING"
            Just st  -> "https://api.mybitx.com/api/1/listorders?state=PENDING&pair=" ++ show st

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

