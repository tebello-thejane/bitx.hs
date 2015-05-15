{-# LANGUAGE OverloadedStrings, QuasiQuotes, DataKinds #-}

module Network.Bitcoin.BitX.Private.Auth
    (
    authGrant
    ) where

import Network.Bitcoin.BitX.Types
import Network.Bitcoin.BitX.Types.Internal
import Network.Bitcoin.BitX.Internal
import qualified Network.HTTP.Conduit as NetCon
import Network.HTTP.Conduit (Response(..))
import Control.Exception (try, SomeException)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)
import Network (withSocketsDo)
import Record (lens)
import Record.Lens (view)
import qualified Data.Text.Encoding as Txt
import Data.Text (Text)

{- | Grant

Once your application has received an authorization code, it can exchange it for an API key. This is
done by calling the grant endpoint. The resulting API key can be used to access the BitX API calls
for which it has the appropriate permissions.
-}

authGrant :: BitXClientAuth -> Text -> IO (Maybe (Either BitXError BitXAuth))
authGrant cauth authCode = withSocketsDo $ do
    response <- try . NetCon.withManager . NetCon.httpLbs . NetCon.applyBasicAuth
          userID
          userSecret
        . NetCon.urlEncodedBody
            [("grant_type", "authorization_code"),
             ("code", showableToBytestring_ authCode)]
        . fromJust . NetCon.parseUrl $ "https://api.mybitx.com/api/oauth2/grant"
        :: IO (Either SomeException (Response BL.ByteString))
    consumeResponseBody_ response
    where
        userID = Txt.encodeUtf8 $ view [lens| id |] cauth
        userSecret = Txt.encodeUtf8 $ view [lens| secret |] cauth
