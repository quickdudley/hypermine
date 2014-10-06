module Craft.Table (
  Recipie (..)
 ) where

import Data.List
import Data.Maybe

import Item

{-
The table surface is a section of order-4 pentagonal tiling: arrangement as
follows:
    a
  b   c
d   e   f
 g     h
  i j k
-}

data Ingredient = Ingredient
  ItemType
  [(String,Condition)]
  deriving (Eq,Show)

data Condition = CFalse | CTrue | CLT | CEQ | CGT deriving (Eq,Show)

data Recipie = Recipie
  (Maybe Ingredient) -- a
  (Maybe Ingredient) -- b
  (Maybe Ingredient) -- c
  (Maybe Ingredient) -- d
  (Maybe Ingredient) -- e
  (Maybe Ingredient) -- f
  (Maybe Ingredient) -- g
  (Maybe Ingredient) -- h
  (Maybe Ingredient) -- i
  (Maybe Ingredient) -- j
  (Maybe Ingredient) -- k

instance Eq Recipie where
  r1 == r2 = r2 `elem` reposition r1

-- I kept making mistakes writing this next part, so I wrote a program to write
-- it for me.
displacefuncs = let z = Nothing in [
  \(Recipie a b c d e f g h i j k) ->
    case catMaybes [f,h,i,j,k] of
      [] -> Just $ Recipie z z z z b z d a g e c
      _ -> Nothing,
  \(Recipie a b c d e f g h i j k) ->
    case catMaybes [d,g,i,j,k] of
      [] -> Just $ Recipie z z z z c z a f b e h
      _ -> Nothing,
  \(Recipie a b c d e f g h i j k) ->
    case catMaybes [a,c,f,h,k] of
      [] -> Just $ Recipie z z d z g b z e z i j
      _ -> Nothing,
  \(Recipie a b c d e f g h i j k) ->
    case catMaybes [a,b,c,d,f] of
      [] -> Just $ Recipie h e k g j z i z z z z
      _ -> Nothing,
  \(Recipie a b c d e f g h i j k) ->
    case catMaybes [a,b,c,d,f] of
      [] -> Just $ Recipie g i e z j h z k z z z
      _ -> Nothing,
  \(Recipie a b c d e f g h i j k) ->
    case catMaybes [a,b,d,g,i] of
      [] -> Just $ Recipie z f z c h z e z j k z
      _ -> Nothing
 ]

reposition :: Recipie -> [Recipie]
reposition o = rp [] [o] where
  rp a c = let
    a' = a ++ c
    in c ++ rp a'
      (deleteFirstsBy exactRecipie 
         (concatMap (\r -> mapMaybe ($ r) displacefuncs) c)
         a'
      )

exactRecipie
  (Recipie a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1)
  (Recipie a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2)
    = (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1) ==
      (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2)

