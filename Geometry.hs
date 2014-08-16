module Geometry () where

import Data.Number.Fixed
import Data.Ratio

data HyperPoint = HP (Fixed Prec50)
  (Fixed Prec50) (Fixed Prec50) (Fixed Prec50) deriving Show

hyperPoint x y z = HP (sqrt (x^2 + y^2 + z^2 + 1)) x y z

bilinear (HP h1 x1 y1 z1) (HP h2 x2 y2 z2) = h1*h2 - x1*x2 - y1*y2 - z1*z2

quadratic p = bilinear p p

distance x y = acosh (bilinear x y)

angle x y z = let
  a = distance x y
  b = distance x z
  c = distance y z
  in acos ((cosh a * cosh b - cosh c) / (sinh a * sinh b))

{-
l = acosh (sqrt (x^2 + 1))
cosh l = sqrt (x^2 + 1)
(cosh l)^2 = x^2 + 1
(cosh l)^2 - 1 = x^2
sqrt ((cosh l)^2 - 1) = x
-}
