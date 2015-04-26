module Network.Bitcoin.BitX.Private
  (
  ) where

import Network.Bitcoin.BitX.Types

{- | Returns a list of the most recently placed orders.

This list is truncated after 100 items. -}

getAllOrders :: BitXAuth -> Maybe CcyPair -> IO [PrivateOrder]
getAllOrders = undefined

{- | Create a new order. -}

postOrder :: BitXAuth -> OrderRequest -> IO OrderID
postOrder = undefined

{- | Request to stop an order. -}

stopOrder :: BitXAuth -> OrderID -> IO StopOrderSuccess
