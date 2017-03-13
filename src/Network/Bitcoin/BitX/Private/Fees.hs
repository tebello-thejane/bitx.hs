{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Bitcoin.BitX.Private.Fees
-- Copyright   :  2017 Tebello Thejane
-- License     :  BSD3
--
-- Maintainer  :  Tebello Thejane <zyxoas+hackage@gmail.com>
-- Stability   :  Experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Retrieving fee information.
--
-- The BitX fee structure is cummulative-volume-based. The sole endpoint in
-- this package returns the user's current fee structure and 30-day volume.
--
-----------------------------------------------------------------------------

module Network.Bitcoin.BitX.Private.Fees
    (
    getFeeInfo
    ) where

import Network.Bitcoin.BitX.Internal
import Network.Bitcoin.BitX.Types
import Data.Text as Txt
import Network.Bitcoin.BitX.Response
import Data.Monoid ((<>))

{- | Get fee structure and 30-day volume (as of midnight) for a currency pair

@Perm_R_Orders@ permission required.
-}

getFeeInfo :: BitXAuth -> CcyPair -> IO (BitXAPIResponse FeeInfo)
getFeeInfo auth cpair = simpleBitXGetAuth_ auth ("fee_info?pair=" <> Txt.pack (show cpair))
