{-# LANGUAGE OverloadedStrings, QuasiQuotes, DataKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Bitcoin.BitX.Private.Auth
-- Copyright   :  2015 Tebello Thejane
-- License     :  BSD3
--
-- Maintainer  :  Tebello Thejane <zyxoas+hackage@gmail.com>
-- Stability   :  Experimental
-- Portability :  non-portable (GHC Extensions)
--
-- The single function in this module, allows your application to use the
-- private BitX API via OAuth2.
--
-- To use OAuth2, you first need to contact the BitX team <support@bitx.co>,
-- and provide them with
--
-- * Application name
-- * Application description
-- * Website URL
-- * Permissions required
-- * Redirect URL
-- * Your BitX account username
--
-- Next, the user needs to be sent to <https://bitx.co/oauth2/authorize?client_id=your_client_id&scope=your_requested_permissions&state=your_unique_state>,
-- where they can confirm the request. If they do confirm the request, they
-- will be sent to your redirect URL, with an authorisation code as one of
-- the GET parameter: <https://example.com/callback?code=authorization_code&state=your_unique_state>
--
-- The permissions are explained in the "Network.Bitcoin.BitX.Private" module.
--
-----------------------------------------------------------------------------

module Network.Bitcoin.BitX.Private.Auth
    (
    authGrant
    ) where

import Network.Bitcoin.BitX.Types
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
import Network.Bitcoin.BitX.Response

{- | Grant

Once your application has received an authorization code, it can exchange it for an API key. This is
done by calling the grant endpoint. The resulting API key can be used to access the BitX API calls
for which it has the appropriate permissions.
-}

authGrant :: BitXClientAuth -> Text -> IO (BitXAPIResponse BitXAuth)
authGrant cauth authCode = withSocketsDo $ do
    response <- try . NetCon.withManager . NetCon.httpLbs . NetCon.applyBasicAuth
          userID
          userSecret
        . NetCon.urlEncodedBody
            [("grant_type", "authorization_code"),
             ("code", Txt.encodeUtf8 authCode)]
        . fromJust . NetCon.parseUrl $ bitXAPIPrefix ++ "oauth2/grant"
        :: IO (Either SomeException (Response BL.ByteString))
    return $ consumeResponseBody_ response
    where
        userID = Txt.encodeUtf8 $ view [lens| id |] cauth
        userSecret = Txt.encodeUtf8 $ view [lens| secret |] cauth
