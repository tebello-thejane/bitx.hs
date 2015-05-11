{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module Network.Bitcoin.BitX.Internal
    (
    simpleBitXGetAuth_,
    simpleBitXGet_,
    simpleBitXPOSTAuth_
    )
where

import Network.Bitcoin.BitX.Types
import Network.Bitcoin.BitX.Types.Internal
import qualified Network.HTTP.Conduit as NetCon
import Network.HTTP.Conduit (Response(..))
import Control.Exception (try, SomeException)
import qualified Data.Aeson as Aeson (decode)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)
import Network (withSocketsDo)
import Record (lens)
import Record.Lens (view)
import qualified Data.Text.Encoding as Txt

bitXAPIRoot :: String
bitXAPIRoot = "https://api.mybitx.com/api/1/"

simpleBitXGetAuth_ :: BitXAesRecordConvert rec aes => BitXAuth -> String -> IO (Maybe (Either BitXError rec))
simpleBitXGetAuth_ auth verb = withSocketsDo $ do
    response <- try . NetCon.withManager . NetCon.httpLbs . NetCon.applyBasicAuth
          userID
          userSecret
        . fromJust . NetCon.parseUrl $ (bitXAPIRoot ++ verb)
        :: IO (Either SomeException (Response BL.ByteString))
    consumeResponseBody response
    where
        userID = Txt.encodeUtf8 $ (view [lens| id |] auth)
        userSecret = Txt.encodeUtf8 $ (view [lens| secret |] auth)

simpleBitXPOSTAuth_ :: (BitXAesRecordConvert rec aes, POSTEncodeable inprec) => BitXAuth -> inprec
    -> String -> IO (Maybe (Either BitXError rec))
simpleBitXPOSTAuth_ auth encrec verb = withSocketsDo $ do
    response <- try . NetCon.withManager . NetCon.httpLbs . NetCon.applyBasicAuth
          userID
          userSecret
        . NetCon.urlEncodedBody (postEncode encrec)
        . fromJust . NetCon.parseUrl $ (bitXAPIRoot ++ verb)
        :: IO (Either SomeException (Response BL.ByteString))
    consumeResponseBody response
    where
        userID = Txt.encodeUtf8 $ (view [lens| id |] auth)
        userSecret = Txt.encodeUtf8 $ (view [lens| secret |] auth)

simpleBitXGet_ :: BitXAesRecordConvert rec aes => String -> IO (Maybe (Either BitXError rec))
simpleBitXGet_ verb = withSocketsDo $ do
    resp <- try $ NetCon.simpleHttp (bitXAPIRoot ++ verb)
        :: IO (Either SomeException BL.ByteString)
    consumeResponse resp

consumeResponse :: BitXAesRecordConvert rec aes => Either SomeException BL.ByteString -> IO (Maybe (Either BitXError rec))
consumeResponse resp =
    case resp of
        Left _  -> return Nothing -- gobble up all exceptions and just return Nothing
        Right k -> bitXErrorOrPayload k

consumeResponseBody :: BitXAesRecordConvert rec aes => Either SomeException (NetCon.Response BL.ByteString)
    -> IO (Maybe (Either BitXError rec))
consumeResponseBody resp =
    case resp of
        Left _  -> return Nothing -- gobble up all exceptions and just return Nothing
        Right k -> bitXErrorOrPayload $ NetCon.responseBody k

bitXErrorOrPayload :: BitXAesRecordConvert rec aes => BL.ByteString -> IO (Maybe (Either BitXError rec))
bitXErrorOrPayload body = do
            let respTE = (Aeson.decode $ body) -- is it a BitX error?
            case respTE of
                Just e  -> return (Just (Left (aesToRec e)))
                Nothing -> do
                    let respTT = (Aeson.decode $ body)
                    case respTT of
                        Just t  -> return (Just (Right (aesToRec t)))
                        Nothing -> return Nothing
