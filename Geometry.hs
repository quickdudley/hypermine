module Geometry () where

--import Data.Monoid
import Data.Number.Fixed
import Data.Ratio

data Axis = H | X | Y | Z deriving (Ord,Enum,Show,Read)

data HyperPoint = HP (Fixed Prec50)
  (Fixed Prec50) (Fixed Prec50) (Fixed Prec50) deriving Show

data HyperLine = HL HyperPoint HyperPoint deriving Show

hyperPoint x y z = HP (sqrt (x^2 + y^2 + z^2 + 1)) x y z


bilinear (HP h1 x1 y1 z1) (HP h2 x2 y2 z2) = h1*h2 - x1*x2 - y1*y2 - z1*z2

quadratic p = bilinear p p

distance x y = acosh (bilinear x y)

angle x y z = let
  a = distance x y
  b = distance x z
  c = distance y z
  in acos ((cosh a * cosh b - cosh c) / (sinh a * sinh b))

joinPoints :: HyperPoint -> HyperPoint -> HyperLine
joinPoints = HL

extrapolate (HL p2@(HP h2 x2 y2 z2) p1@(HP h1 x1 y1 z1)) a = let
  bilin = bilinear p2 p1
  q1 = quadratic p1
  q2 = quadratic p2
  sqrpt = sqrt ((2 * a * bilin)^2 - 4*q2*(q1*(a^2) - 1))
  b = (-2 * a * bilin + sqrpt) /
       (2*q2)
  in hyperPoint
    (x1 * a + x2 * b) (y1 * a + y2 * b) (z1 * a + z2 * b)

-- This function is not as useful as it seems because rounding errors can
-- give both false positives (occasionally) and false negatives (often)
onLine (HL (HP h1 x1 y1 z1) (HP h2 x2 y2 z2)) (HP h x y z) = let
  d = h1 * x2 - h2 * x1
  a = (h * x2 - h2 * x) / d
  b = (h1 * x - h * x1) / d
  in y == a*y1 + b * y2 && z == a * z1 + b * z2

axis a l = let
  x = sqrt ((cosh l)^2 - 1)
  in case a of
    H -> hyperPoint 0 0 0
    X -> hyperPoint x 0 0
    Y -> hyperPoint 0 x 0
    Z -> hyperPoint 0 0 x

