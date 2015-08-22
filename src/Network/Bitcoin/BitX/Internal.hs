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
import qualified Network.HTTP.Conduit as NetCon
import Network.HTTP.Types (status503)
import Network.HTTP.Conduit (Response(..), Request(..))
import Control.Exception (try)
import qualified Data.Aeson as Aeson (decode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import Network (withSocketsDo)
import qualified Data.Text.Encoding as Txt
import Network.Bitcoin.BitX.Response
import Control.Applicative ((<|>))
import Lens.Micro ((^.))
import Control.Concurrent (threadDelay)
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

authConnect :: BitXAuth -> Request -> IO (Either NetCon.HttpException (Response BL.ByteString))
authConnect auth req = do
    manager <- globalManager
    try . flip NetCon.httpLbs manager . NetCon.applyBasicAuth userID userSecret $ req
    where
        userID = Txt.encodeUtf8 (auth ^. Types.id)
        userSecret = Txt.encodeUtf8 (auth ^. Types.secret)

simpleBitXGetAuth_ :: BitXAesRecordConvert recd aes => BitXAuth -> String -> IO (BitXAPIResponse recd)
simpleBitXGetAuth_ auth verb = withSocketsDo $
    rateLimit
        (authConnect auth
            . fromJust . NetCon.parseUrl $ (bitXAPIRoot ++ verb))
        consumeResponseIO

simpleBitXPOSTAuth_ :: (BitXAesRecordConvert recd aes, POSTEncodeable inprec) => BitXAuth -> inprec
    -> String -> IO (BitXAPIResponse recd)
simpleBitXPOSTAuth_ auth encrec verb = withSocketsDo $
    rateLimit
        (authConnect auth
            . NetCon.urlEncodedBody (postEncode encrec)
            . fromJust . NetCon.parseUrl $ (bitXAPIRoot ++ verb))
        consumeResponseIO

simpleBitXMETHAuth_ :: BitXAesRecordConvert recd aes => BitXAuth -> BS.ByteString
    -> String -> IO (BitXAPIResponse recd)
simpleBitXMETHAuth_ auth meth verb = withSocketsDo $
    rateLimit
        (authConnect auth (fromJust (NetCon.parseUrl (bitXAPIRoot ++ verb))) { method = meth })
        consumeResponseIO

simpleBitXGet_ :: BitXAesRecordConvert recd aes => String -> IO (BitXAPIResponse recd)
simpleBitXGet_ verb = withSocketsDo $ do
    manager <- globalManager
    rateLimit
        (try . flip NetCon.httpLbs manager . fromJust . NetCon.parseUrl $ (bitXAPIRoot ++ verb))
        consumeResponseIO

rateLimit :: IO (Either NetCon.HttpException c) -> (Either NetCon.HttpException c -> IO d) -> IO d
rateLimit act1 act2 = go 500000
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

consumeResponseIO :: BitXAesRecordConvert recd aes => Either NetCon.HttpException (NetCon.Response BL.ByteString)
    -> IO (BitXAPIResponse recd)
consumeResponseIO resp =
    return $ case resp of
        Left ex -> ExceptionResponse ex
        Right k -> bitXErrorOrPayload k

bitXErrorOrPayload :: BitXAesRecordConvert recd aes => Response BL.ByteString -> BitXAPIResponse recd
bitXErrorOrPayload resp = fromJust $
        ErrorResponse . aesToRec <$> Aeson.decode body -- is it a BitX error?
    <|> ValidResponse . aesToRec <$> Aeson.decode body
    <|> Just (UnparseableResponse  resp)
    where
        body = NetCon.responseBody resp

isRateLimited :: Either NetCon.HttpException a -> Bool
isRateLimited (Left  (NetCon.StatusCodeException st _ _)) = st == status503
isRateLimited _ = False
