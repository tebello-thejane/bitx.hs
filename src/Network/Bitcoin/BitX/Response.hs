{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Bitcoin.BitX.Response
-- Copyright   :  No Rights Reserved
-- License     :  Public Domain
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

import Network.HTTP.Conduit (Response(..))
import Data.ByteString.Lazy (ByteString)
import Network.Bitcoin.BitX.Types
import Data.Text (Text)

-- | This retun type enumerates all possible failure modes.

data BitXAPIResponse rec =
      ExceptionResponse Text -- ^ Some exception occured while making the call to BitX, and this was
                             -- the exception text.
    | ErrorResponse BitXError -- ^ BitX returned an error record instead of returning the data we
                              -- were expecting.
    | ValidResponse rec -- ^ We received the data type we were expecting.
    | UnparseableResponse (Response ByteString) -- ^ BitX retuned data which couldn't be parsed,
                                                -- such as some text which was probably not JSON format.

deriving instance Show rec => Show (BitXAPIResponse rec)
deriving instance Eq rec => Eq (BitXAPIResponse rec)
