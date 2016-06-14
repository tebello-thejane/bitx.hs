{-# LANGUAGE OverloadedStrings, DataKinds, CPP #-}

module Network.Bitcoin.BitX.Internal
    (
    simpleBitXGetAuth_,
    simpleBitXGet_,
    simpleBitXPOSTAuth_,
    simpleBitXMETHAuth_,
    bitXAPIPrefix
    )
where

import Network.Bitcoin.BitX.Types
import qualified Network.Bitcoin.BitX.Types as Types
import Network.Bitcoin.BitX.Types.Internal
import qualified Network.HTTP.Client as NetCon
import qualified Network.HTTP.Client.TLS as NetCon
import Network.HTTP.Types.Status (status503, status429)
import Network.HTTP.Client (Response(..), Request(..))
import Control.Exception (try)
import qualified Data.Aeson as Aeson (decode, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import Network (withSocketsDo)
import qualified Data.Text.Encoding as Txt
import qualified Data.Text as Txt
import Network.Bitcoin.BitX.Response
import Lens.Micro ((^.))
import Control.Concurrent (threadDelay)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Monoid ((<>))
#if __GLASGOW_HASKELL__ >= 710
-- <$> is in base since 4.8 (GHC 7.10) due to the AMP
import Control.Applicative ((<|>))
#else
import Control.Applicative ((<|>), (<$>))
#endif

bitXAPIPrefix :: Text
bitXAPIPrefix = "https://api.mybitx.com/api/"

bitXAPIRoot :: Text
bitXAPIRoot = bitXAPIPrefix <> "1/"

globalManager :: IO NetCon.Manager
globalManager = NetCon.newManager NetCon.tlsManagerSettings

authConnect :: BitXAuth -> Request -> IO (Either NetCon.HttpException (Response BL.ByteString))
authConnect auth req = do
    manager <- globalManager
    try . flip NetCon.httpLbs manager . NetCon.applyBasicAuth userID userSecret $ req
    where
        userID = Txt.encodeUtf8 (auth ^. Types.id)
        userSecret = Txt.encodeUtf8 (auth ^. Types.secret)

simpleBitXGetAuth_ :: BitXAesRecordConvert recd => BitXAuth -> Text -> IO (BitXAPIResponse recd)
simpleBitXGetAuth_ auth verb = withSocketsDo $
    rateLimit
        (authConnect auth
            . fromJust . NetCon.parseUrl $ Txt.unpack (bitXAPIRoot <> verb))
        consumeResponseIO

simpleBitXPOSTAuth_ :: (BitXAesRecordConvert recd, POSTEncodeable inprec) => BitXAuth -> inprec
    -> Text -> IO (BitXAPIResponse recd)
simpleBitXPOSTAuth_ auth encrec verb = withSocketsDo $
    rateLimit
        (authConnect auth
            . NetCon.urlEncodedBody (postEncode encrec)
            . fromJust . NetCon.parseUrl $ Txt.unpack (bitXAPIRoot <> verb))
        consumeResponseIO

simpleBitXMETHAuth_ :: BitXAesRecordConvert recd => BitXAuth -> BS.ByteString
    -> Text -> IO (BitXAPIResponse recd)
simpleBitXMETHAuth_ auth meth verb = withSocketsDo $
    rateLimit
        (authConnect auth (fromJust (NetCon.parseUrl $ Txt.unpack (bitXAPIRoot <> verb))) { method = meth })
        consumeResponseIO

simpleBitXGet_ :: BitXAesRecordConvert recd => Text -> IO (BitXAPIResponse recd)
simpleBitXGet_ verb = withSocketsDo $ do
    manager <- globalManager
    rateLimit
        (try . flip NetCon.httpLbs manager . fromJust . NetCon.parseUrl $ Txt.unpack (bitXAPIRoot <> verb))
        consumeResponseIO

rateLimit :: IO (Either NetCon.HttpException c) -> (Either NetCon.HttpException c -> IO d) -> IO d
rateLimit act1 act2 = go (500 * 1000)
    where
        go del = do
            resp <- act1
            if isRateLimited resp then
                if del > maxLimit
                    then act2 resp
                    else do
                        threadDelay del
                        go (incDelay del)
            else act2 resp
        maxLimit = 5 * 1000 * 1000 -- 5 seconds probably means something else is wrong...
        incDelay = round . (* (1.5 :: Double)) . fromIntegral

consumeResponseIO :: BitXAesRecordConvert recd => Either NetCon.HttpException (NetCon.Response BL.ByteString)
    -> IO (BitXAPIResponse recd)
consumeResponseIO resp =
    return $ case resp of
        Left ex -> ExceptionResponse ex
        Right k -> bitXErrorOrPayload k

bitXErrorOrPayload :: BitXAesRecordConvert recd => Response BL.ByteString -> BitXAPIResponse recd
bitXErrorOrPayload resp = fromJust $
        ErrorResponse . aesToRec <$> Aeson.decode body -- is it a BitX error?
    <|> ValidResponse . aesToRec <$> eitherToMaybe respBody -- I can't get the typechecker to work if I use "Aeson.decode body" here
    <|> Just (UnparseableResponse aesonErr $ fmap (decodeUtf8 . toStrict) resp)
    where
        aesonErr = pack $ fromLeft respBody
        respBody = Aeson.eitherDecode body
        body = NetCon.responseBody resp

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft     _    = Prelude.error "fromLeft called on Right value. This is not supposed to happen..."

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right b) = Just b

isRateLimited :: Either NetCon.HttpException a -> Bool
isRateLimited (Left  (NetCon.StatusCodeException st _ _)) = st == status503 || st == status429
isRateLimited _ = False
