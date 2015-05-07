module Network.Bitcoin.BitX.Private
  (
  getAllOrders,
  postOrder,
  stopOrder,
  getPendingOrders
  ) where

import Network.Bitcoin.BitX.Internal
import Network.Bitcoin.BitX.Types
import qualified Network.HTTP.Conduit as NetCon
import Network.HTTP.Conduit (Response(..))
import Control.Exception (try, SomeException)
import qualified Data.Aeson as Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)
import Network (withSocketsDo)
import Network.Bitcoin.BitX.Types.Internal
import Record (lens)
import Record.Lens (view)
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as Txt

{- | Returns a list of the most recently placed orders.

If the second parameter is @Nothing@ then this will return orders for all markets, whereas if it is
@Just cpy@ for some @CcyPair cpy@ then the results will be specific to that market.

This list is truncated after 100 items. -}

getAllOrders :: BitXAuth -> Maybe CcyPair -> IO (Maybe (Either BitXError PrivateOrders))
getAllOrders auth pair = simpleBitXGetAuth_ auth url
    where
        url = case pair of
            Nothing  -> "listorders"
            Just st  -> "listorders?pair=" ++ show st

{- | Returns a list of the most recently placed orders which are still in 'PENDING' state.

If the second parameter is @Nothing@ then this will return orders for all markets, whereas if it is
@Just cpy@ for some @CcyPair cpy@ then the results will be specific to that market.

This list is truncated after 100 items. -}

getPendingOrders :: BitXAuth -> Maybe CcyPair -> IO (Maybe (Either BitXError PrivateOrders))
getPendingOrders auth pair = simpleBitXGetAuth_ auth url
    where
        url = case pair of
            Nothing  -> "listorders?state=PENDING"
            Just st  -> "listorders?state=PENDING&pair=" ++ show st

{- | Create a new order. -}

postOrder :: BitXAuth -> OrderRequest -> IO OrderID
postOrder = undefined

{- | Request to stop an order. -}

stopOrder :: BitXAuth -> OrderID -> IO StopOrderSuccess
stopOrder = undefined

