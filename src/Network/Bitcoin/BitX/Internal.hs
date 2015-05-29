{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, DataKinds #-}

module Network.Bitcoin.BitX.Internal
    (
    simpleBitXGetAuth_,
    simpleBitXGet_,
    simpleBitXPOSTAuth_,
    simpleBitXMETHAuth_,
    consumeResponseBody_,
    bitXAPIPrefix
    )
where

import Network.Bitcoin.BitX.Types
import Network.Bitcoin.BitX.Types.Internal
import qualified Network.HTTP.Conduit as NetCon
import Network.HTTP.Conduit (Response(..), Request(..))
import Control.Exception (try, SomeException)
import qualified Data.Aeson as Aeson (decode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import Network (withSocketsDo)
import Record (lens)
import Record.Lens (view)
import qualified Data.Text.Encoding as Txt
import qualified Data.Text as Txt
import Network.Bitcoin.BitX.Response

bitXAPIPrefix :: String
bitXAPIPrefix = "https://api.mybitx.com/api/"

bitXAPIRoot :: String
bitXAPIRoot = bitXAPIPrefix ++ "1/"

simpleBitXGetAuth_ :: BitXAesRecordConvert rec aes => BitXAuth -> String -> IO (BitXAPIResponse rec)
simpleBitXGetAuth_ auth verb = withSocketsDo $ do
    response <- try . NetCon.withManager . NetCon.httpLbs . NetCon.applyBasicAuth
          userID
          userSecret
        . fromJust . NetCon.parseUrl $ (bitXAPIRoot ++ verb)
        :: IO (Either SomeException (Response BL.ByteString))
    consumeResponseBody_ response
    where
        userID = Txt.encodeUtf8 $ (view [lens| id |] auth)
        userSecret = Txt.encodeUtf8 $ (view [lens| secret |] auth)

simpleBitXPOSTAuth_ :: (BitXAesRecordConvert rec aes, POSTEncodeable inprec) => BitXAuth -> inprec
    -> String -> IO (BitXAPIResponse rec)
simpleBitXPOSTAuth_ auth encrec verb = withSocketsDo $ do
    response <- try . NetCon.withManager . NetCon.httpLbs . NetCon.applyBasicAuth
          userID
          userSecret
        . NetCon.urlEncodedBody (postEncode encrec)
        . fromJust . NetCon.parseUrl $ (bitXAPIRoot ++ verb)
        :: IO (Either SomeException (Response BL.ByteString))
    consumeResponseBody_ response
    where
        userID = Txt.encodeUtf8 $ (view [lens| id |] auth)
        userSecret = Txt.encodeUtf8 $ (view [lens| secret |] auth)

simpleBitXMETHAuth_ :: BitXAesRecordConvert rec aes => BitXAuth -> BS.ByteString
    -> String -> IO (BitXAPIResponse rec)
simpleBitXMETHAuth_ auth meth verb = withSocketsDo $ do
    let initReq = (fromJust (NetCon.parseUrl $ (bitXAPIRoot ++ verb))) { method = meth }
    response <- try . NetCon.withManager . NetCon.httpLbs . NetCon.applyBasicAuth
          userID
          userSecret $ initReq
        :: IO (Either SomeException (Response BL.ByteString))
    consumeResponseBody_ response
    where
        userID = Txt.encodeUtf8 $ (view [lens| id |] auth)
        userSecret = Txt.encodeUtf8 $ (view [lens| secret |] auth)

simpleBitXGet_ :: BitXAesRecordConvert rec aes => String -> IO (BitXAPIResponse rec)
simpleBitXGet_ verb = withSocketsDo $ do
    resp <- try . NetCon.withManager . NetCon.httpLbs
        . fromJust . NetCon.parseUrl $ (bitXAPIRoot ++ verb)
        :: IO (Either SomeException (Response BL.ByteString))
    consumeResponse resp

consumeResponse :: BitXAesRecordConvert rec aes => Either SomeException (NetCon.Response BL.ByteString)
    -> IO (BitXAPIResponse rec)
consumeResponse resp =
    case resp of
        Left ex -> return $ ExceptionResponse . Txt.pack . show $ ex
        Right k -> bitXErrorOrPayload k

consumeResponseBody_ :: BitXAesRecordConvert rec aes => Either SomeException (NetCon.Response BL.ByteString)
    -> IO (BitXAPIResponse rec)
consumeResponseBody_ resp =
    case resp of
        Left ex -> return $ ExceptionResponse . Txt.pack . show $ ex
        Right k -> bitXErrorOrPayload k

bitXErrorOrPayload :: BitXAesRecordConvert rec aes => Response BL.ByteString -> IO (BitXAPIResponse rec)
bitXErrorOrPayload resp = do
            let respTE = Aeson.decode body -- is it a BitX error?
            case respTE of
                Just e  -> return . ErrorResponse . aesToRec $ e
                Nothing -> do
                    let respTT = Aeson.decode body
                    case respTT of
                        Just t  -> return . ValidResponse . aesToRec $ t
                        Nothing -> return . UnparseableResponse $ resp
            where
                body = NetCon.responseBody resp

