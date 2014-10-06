module Craft.Common (
  Condition(..),
  Ingredient,
  matchIngredient
 ) where
import qualified Data.Map as M

import Item

data Condition = CFalse | CTrue |
  CLT Integer | CEQ Integer | CGT Integer deriving (Eq,Show)

data Ingredient = Ingredient
  ItemType
  [(String,Condition)]
  deriving (Eq,Show)

matchIngredient (Ingredient t cs) (Item t' a) = (t == (-1) || t == t') &&
  all (\(n,c) -> matchCondition c (M.lookup n a)) cs

matchCondition CFalse Nothing = True
matchCondition CFalse (Just (Left False)) = True
matchCondition CFalse (Just (Right 0)) = True
matchCondition CFalse _ = False
matchCondition CTrue i = not $ matchCondition CFalse i
matchCondition (CLT _) Nothing = False
matchCondition (CLT _) (Just (Left _)) = False
matchCondition (CLT s) (Just (Right v)) = v < s
matchCondition (CEQ _) Nothing = False
matchCondition (CEQ _) (Just (Left _)) = False
matchCondition (CEQ s) (Just (Right v)) = v == s
matchCondition (CGT _) Nothing = False
matchCondition (CGT _) (Just (Left _)) = False
matchCondition (CGT s) (Just (Right v)) = v > s

