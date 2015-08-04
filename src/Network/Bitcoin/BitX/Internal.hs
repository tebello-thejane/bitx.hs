{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, DataKinds, CPP #-}

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
import qualified Network.Bitcoin.BitX.Types as Types
import Network.Bitcoin.BitX.Types.Internal
import qualified Network.HTTP.Conduit as NetCon
--import Network.HTTP.Types (status503)
import Network.HTTP.Conduit (Response(..), Request(..))
import Control.Exception (try, SomeException)
import qualified Data.Aeson as Aeson (decode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import Network (withSocketsDo)
import qualified Data.Text.Encoding as Txt
import qualified Data.Text as Txt
import Network.Bitcoin.BitX.Response
import Control.Applicative ((<|>))
import Lens.Micro
#if MIN_VERSION_base(4,8,0)
-- <$> is in base since 4.8 due to the AMP
#else
import Control.Applicative ((<$>))
#endif

bitXAPIPrefix :: String
bitXAPIPrefix = "https://api.mybitx.com/api/"

bitXAPIRoot :: String
bitXAPIRoot = bitXAPIPrefix ++ "1/"

globalManager :: IO NetCon.Manager
globalManager = NetCon.newManager NetCon.tlsManagerSettings

authConnect :: BitXAuth -> NetCon.Manager -> Request -> IO (Either SomeException (Response BL.ByteString))
authConnect auth manager = do
    try . (flip NetCon.httpLbs) manager . NetCon.applyBasicAuth userID userSecret
    where
        userID = Txt.encodeUtf8 $ (auth ^. Types.id)
        userSecret = Txt.encodeUtf8 $ (auth ^. Types.secret)

simpleBitXGetAuth_ :: BitXAesRecordConvert rec aes => BitXAuth -> String -> IO (BitXAPIResponse rec)
simpleBitXGetAuth_ auth verb = withSocketsDo $ do
    manager <- globalManager
    response <- authConnect auth manager
        . fromJust . NetCon.parseUrl $ (bitXAPIRoot ++ verb)
        :: IO (Either SomeException (Response BL.ByteString))
    return $ consumeResponseBody_ response

simpleBitXPOSTAuth_ :: (BitXAesRecordConvert rec aes, POSTEncodeable inprec) => BitXAuth -> inprec
    -> String -> IO (BitXAPIResponse rec)
simpleBitXPOSTAuth_ auth encrec verb = withSocketsDo $ do
    manager <- globalManager
    response <- authConnect auth manager
        . NetCon.urlEncodedBody (postEncode encrec)
        . fromJust . NetCon.parseUrl $ (bitXAPIRoot ++ verb)
        :: IO (Either SomeException (Response BL.ByteString))
    return $ consumeResponseBody_ response

simpleBitXMETHAuth_ :: BitXAesRecordConvert rec aes => BitXAuth -> BS.ByteString
    -> String -> IO (BitXAPIResponse rec)
simpleBitXMETHAuth_ auth meth verb = withSocketsDo $ do
    let initReq = (fromJust (NetCon.parseUrl $ (bitXAPIRoot ++ verb))) { method = meth }
    manager <- globalManager
    response <- authConnect auth manager initReq
        :: IO (Either SomeException (Response BL.ByteString))
    return $ consumeResponseBody_ response

simpleBitXGet_ :: BitXAesRecordConvert rec aes => String -> IO (BitXAPIResponse rec)
simpleBitXGet_ verb = withSocketsDo $ do
    manager <- globalManager
    resp <- try . (flip NetCon.httpLbs) manager
        . fromJust . NetCon.parseUrl $ (bitXAPIRoot ++ verb)
        :: IO (Either SomeException (Response BL.ByteString))
    return $ consumeResponse resp

consumeResponse :: BitXAesRecordConvert rec aes => Either SomeException (NetCon.Response BL.ByteString)
    -> BitXAPIResponse rec
consumeResponse resp =
    case resp of
        Left ex -> ExceptionResponse . Txt.pack . show $ ex
        Right k -> bitXErrorOrPayload k

consumeResponseBody_ :: BitXAesRecordConvert rec aes => Either SomeException (NetCon.Response BL.ByteString)
    -> BitXAPIResponse rec
consumeResponseBody_ resp =
    case resp of
        Left ex -> ExceptionResponse . Txt.pack . show $ ex
        Right k -> bitXErrorOrPayload k

bitXErrorOrPayload :: BitXAesRecordConvert rec aes => Response BL.ByteString -> BitXAPIResponse rec
bitXErrorOrPayload resp = fromJust $
        ErrorResponse . aesToRec <$> Aeson.decode body -- is it a BitX error?
    <|> ValidResponse . aesToRec <$> Aeson.decode body
    <|> Just (UnparseableResponse  resp)
    where
        body = NetCon.responseBody resp

--isRateLimited :: Either SomeException (NetCon.Response BL.ByteString) -> Bool
--isRateLimited (Left  _) = False
--isRateLimited (Right r) = (== status503) . NetCon.responseStatus $ r
