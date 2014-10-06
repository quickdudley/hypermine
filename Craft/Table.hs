module Craft.Table (
  Table,
  fromList,
  toList
 ) where

import Data.Function
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad (join)

import Item
import Craft.Common

{-
The table surface is a section of order-4 pentagonal tiling: arrangement as
follows:
    a
  b   c
d   e   f
 g     h
  i j k
-}

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

fromList [e] = Table Nothing
  Nothing Nothing
  Nothing e Nothing
  Nothing Nothing
  Nothing Nothing Nothing
fromList [g,e,i,j] = Table Nothing
  Nothing Nothing
  Nothing e Nothing
  g Nothing
  i j Nothing
fromList [a,b,c,d,e,f,g,h,i,j,k] = Table a b c d e f g h i j k
fromList _ = error "Invalid size for crafting table: must be 1, 4, or 11"

toList ~(Table a b c d e f g h i j k) = [a,b,c,d,e,f,g,h,i,j,k]

instance Functor Table where
  fmap fn (Table a b c d e f g h i j k) =
    Table (fmap fn a)
      (fmap fn b) (fmap fn c)
      (fmap fn d) (fmap fn e) (fmap fn f)
      (fmap fn g) (fmap fn h)
      (fmap fn i) (fmap fn j) (fmap fn k)

instance Applicative Table where
  pure v = Table
    (Just v)
    (Just v) (Just v)
    (Just v) (Just v) (Just v)
    (Just v) (Just v)
    (Just v) (Just v) (Just v)
  (Table a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1) <*>
   (Table a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2) =
    Table (a1 <*> a2)
      (b1 <*> b2) (c1 <*> c2)
      (d1 <*> d2) (e1 <*> e2) (f1 <*> f2)
      (g1 <*> g2) (h1 <*> h2)
      (i1 <*> i2) (j1 <*> j2) (k1 <*> k2)

instance (Eq t) => Eq (Table t) where
  r1 == r2 = any (exactTable r2) $ reposition r1

type Recipie = Table (Ingredient,Maybe Item)

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
reposition = map fst . reposition'

reposition' o = rp 0 [] [o] where
  rp _ _ [] = []
  rp d a c = let
    a' = a ++ c
    in (zip c $ repeat d) ++ rp (d + 1) a'
      (nubBy exactTable $ deleteFirstsBy exactTable 
         (concatMap (\r -> mapMaybe ($ r) displacefuncs) c)
         a'
      )

exactTable :: (Eq t) => Table t -> Table t -> Bool
exactTable
  (Table a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1)
  (Table a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2)
    = (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1) ==
      (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2)

doCraft :: [Recipie] -> Table Item -> Maybe (Table Item)
doCraft recipies input = let
  rt = map fst $ grt $
    map (groupBy ((==) `on` snd) . reposition') recipies where
      grt l = concatMap head l ++ grt (filter (not . null) $ map tail l)
  mmi Nothing Nothing = (True, Nothing)
  mmi Nothing _ = (False, Nothing)
  mmi _ Nothing = (False, Nothing)
  mmi (Just (a,o)) (Just b) = (matchIngredient a b, o)
  mt r = let
    m = zipWith mmi (toList r) (toList input)
    in if all fst m
      then Just $ fromList $ map snd m
      else Nothing
  in join $ find isJust $ map mt rt


