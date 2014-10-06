module Item.Common (
  Item (..),
  ItemType
) where

import qualified Data.Map as M

type ItemType = Int

-- Provisional
data Item = Item {
  itemID :: ItemType,
  itemAttrs :: M.Map String (Either Bool Integer)
 } deriving (Eq)

