{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Bitcoin.BitX.Private.Withdrawal
-- Copyright   :  No Rights Reserved
-- License     :  Public Domain
--
-- Maintainer  :  Tebello Thejane <zyxoas+hackage@gmail.com>
-- Stability   :  Experimental
-- Portability :  non-portable (GHC Extensions)
--
-----------------------------------------------------------------------------

module Network.Bitcoin.BitX.Private.Withdrawal
  (
  getWithdrawalRequests,
  newWithdrawalRequest,
  getWithdrawalRequest
  --cancelWithdrawalRequest
  ) where

import Network.Bitcoin.BitX.Internal
import Network.Bitcoin.BitX.Types
import qualified Data.Text as Txt
import Data.Text (Text)
import Network.Bitcoin.BitX.Response

{- | List withdrawal requests

Returns a list of withdrawal requests.

@Perm_R_Withdrawals@ permission required.-}

getWithdrawalRequests :: BitXAuth -> IO (BitXAPIResponse [WithdrawalRequest])
getWithdrawalRequests auth = simpleBitXGetAuth_ auth "withdrawals/"

{- | Request a withdrawal

Creates a new withdrawal request.

@Perm_W_Withdrawals@ permission required.-}

newWithdrawalRequest :: BitXAuth -> NewWithdrawal -> IO (BitXAPIResponse WithdrawalRequest)
newWithdrawalRequest auth nwithd = simpleBitXPOSTAuth_ auth nwithd "withdrawals"

{- | Get the status of a withdrawal request

Returns the status of a particular withdrawal request.

@Perm_R_Withdrawals@ permission required.-}

getWithdrawalRequest :: BitXAuth -> Text -- ^ The withdrawal ID
    -> IO (BitXAPIResponse WithdrawalRequest)
getWithdrawalRequest auth wthid = simpleBitXGetAuth_ auth $ "withdrawals/" ++ Txt.unpack wthid

--{- | Cancel a withdrawal request

--This can only be done if the request is still in state PENDING.

-- @Perm_W_Withdrawals@ permission required.-}

--cancelWithdrawalRequest :: BitXAuth -> String -> IO (Maybe (Either BitXError WithdrawalRequest))
--cancelWithdrawalRequest auth wthid = simpleBitXMETHAuth_ auth "DELETE" $ "withdrawals/" ++ wthid
