module Craft.Table (
  Table (..)
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

data Table t = Table
  (Maybe t) -- a
  (Maybe t) -- b
  (Maybe t) -- c
  (Maybe t) -- d
  (Maybe t) -- e
  (Maybe t) -- f
  (Maybe t) -- g
  (Maybe t) -- h
  (Maybe t) -- i
  (Maybe t) -- j
  (Maybe t) -- k

instance (Eq t) => Eq (Table t) where
  r1 == r2 = any (exactTable r2) $ reposition r1

-- I kept making mistakes writing this next part, so I wrote a program to write
-- it for me.
displacefuncs = let z = Nothing in [
  \(Table a b c d e f g h i j k) ->
    case catMaybes [f,h,i,j,k] of
      [] -> Just $ Table z z z z b z d a g e c
      _ -> Nothing,
  \(Table a b c d e f g h i j k) ->
    case catMaybes [d,g,i,j,k] of
      [] -> Just $ Table z z z z c z a f b e h
      _ -> Nothing,
  \(Table a b c d e f g h i j k) ->
    case catMaybes [a,c,f,h,k] of
      [] -> Just $ Table z z d z g b z e z i j
      _ -> Nothing,
  \(Table a b c d e f g h i j k) ->
    case catMaybes [a,b,c,d,f] of
      [] -> Just $ Table h e k g j z i z z z z
      _ -> Nothing,
  \(Table a b c d e f g h i j k) ->
    case catMaybes [a,b,c,d,f] of
      [] -> Just $ Table g i e z j h z k z z z
      _ -> Nothing,
  \(Table a b c d e f g h i j k) ->
    case catMaybes [a,b,d,g,i] of
      [] -> Just $ Table z f z c h z e z j k z
      _ -> Nothing
 ]

reposition :: (Eq t) => Table t -> [Table t]
reposition o = rp [] [o] where
  rp _ [] = []
  rp a c = let
    a' = a ++ c
    in c ++ rp a'
      (deleteFirstsBy exactTable 
         (concatMap (\r -> mapMaybe ($ r) displacefuncs) c)
         a'
      )

exactTable :: (Eq t) => Table t -> Table t -> Bool
exactTable
  (Table a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1)
  (Table a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2)
    = (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1) ==
      (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2)

