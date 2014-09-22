module Random (
 ) where

import Data.List
import Data.Bits

data Zipper a = Zipper [a] a [a]

instance (Show a) => Show (Zipper a) where
  show (Zipper l m r) = "[" ++ intercalate ", " e ++ "]" where
    e = map show (reverse l) ++ ["_" ++ show m ++ "_"] ++ map show r

instance Functor Zipper where
  fmap f ~(Zipper l m r) = Zipper (map f l) (f m) (map f r)

rule30 l False False = l
rule30 l _ _ = not l

goLeft (Zipper [] m r) = goLeft $ Zipper (reverse r) m []
goLeft (Zipper (l:lr) m r) = Zipper lr l (m:r)
goRight (Zipper l m []) = goRight $ Zipper [] m (reverse l)
goRight (Zipper l m (r:rl)) = Zipper (m:l) r rl

cojoin ~z@(Zipper l m r) = Zipper l' z r' where
  [l',r'] = map (\(gf,ml) -> zipWith const (tail $ iterate gf z) ml)
    [(goLeft,l), (goRight,r)]

coreturn ~(Zipper _ m _) = m

doRule30 z = fmap d $ cojoin z where
  d z' = let
    [l,m,r] = map (coreturn . ($ z')) [goLeft, id, goRight]
    in rule30 l m r

