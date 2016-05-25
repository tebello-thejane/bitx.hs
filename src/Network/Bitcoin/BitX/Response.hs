{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Bitcoin.BitX.Response
-- Copyright   :  2016 Tebello Thejane
-- License     :  BSD3
--
-- Maintainer  :  Tebello Thejane <zyxoas+hackage@gmail.com>
-- Stability   :  Experimental
-- Portability :  non-portable (GHC Extensions)
--
-- The common return type of the API.
--
-----------------------------------------------------------------------------

module Network.Bitcoin.BitX.Response
  (
    BitXAPIResponse(..)
  ) where

import Network.HTTP.Client (Response(..), HttpException)
import Data.ByteString.Lazy (ByteString)
import Network.Bitcoin.BitX.Types

-- | This retun type enumerates all possible failure modes.

data BitXAPIResponse recd =
      ExceptionResponse HttpException -- ^ Some exception occured while making the call to BitX.
    | ErrorResponse BitXError -- ^ BitX returned an error record instead of returning the data we
                              -- were expecting.
    | ValidResponse recd -- ^ We received the data type we were expecting.
    | UnparseableResponse String (Response ByteString) -- ^ BitX retuned data which couldn't be parsed,
                                                       -- such as some text which was probably not JSON format.
                                                       -- The first value is the error given by Aeson upon trying
                                                       -- to parse the response body.

deriving instance Show recd => Show (BitXAPIResponse recd)
