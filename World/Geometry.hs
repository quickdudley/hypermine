{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module World.Geometry () where

--import Data.Monoid
import Data.Number.Fixed
import Data.Ratio

import Data.Monoid

data Axis = X | Y | Z deriving (Eq,Ord,Enum,Show,Read)

data HyperPoint c = HP c c c c deriving Show

data HyperLine c = HL (HyperPoint c) (HyperPoint c) deriving Show

data Isometry c = HI
  c c c c
  c c c c
  c c c c
  c c c c

infixl 6 *>
class Transformable t c where
  (*>) :: Isometry c -> t c -> t c

hyperPoint x y z = HP (sqrt (x^2 + y^2 + z^2 + 1)) x y z

bilinear (HP h1 x1 y1 z1) (HP h2 x2 y2 z2) = h1*h2 - x1*x2 - y1*y2 - z1*z2

quadratic p = bilinear p p

distance x y = acosh (bilinear x y)

edgeLength :: Floating a => a
edgeLength = let
  a = pi / 4
  c = 2 * pi / 5
  in acosh ((cos c + (cos a)^2) / (sin a)^2)

{-
Calculating the edge length:
1: The corners of each pentagon are right-angled
2: If the pentagon is divided into 5 triangles: each triangle has 2 angles
  of Π/4, and one of 2Π/5
3: The relevant trigonometric formula is:
  cosC = -cosA cosB + sinA sinB cosh c
Substitute A for B because they are the same and rearranged:
c = arcosh ((cos C + (cos A)^2) / (sin A)^2)
c = arcosh ((cos 2Π/5 + (cos Π/4)^2) / (sin Π/4)^2)
-}

angle x y z = let
  a = distance x y
  b = distance x z
  c = distance y z
  in acos ((cosh a * cosh b - cosh c) / (sinh a * sinh b))

joinPoints :: HyperPoint c -> HyperPoint c -> HyperLine c
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
    X -> hyperPoint x 0 0
    Y -> hyperPoint 0 x 0
    Z -> hyperPoint 0 0 x

instance (Num c, Floating c) => Transformable HyperPoint c where
  (HI
    hh hx hy hz
    xh xx xy xz
    yh yx yy yz
    zh zx zy zz) *> (HP h x y z) = hyperPoint
      (xh * h + xx * x + xy * y + xz * z)
      (yh * h + yx * x + yy * y + yz * z)
      (zh * h + zx * x + zy * y + zz * z)

instance (Num c) => Transformable Isometry c where
  (HI
    hh1 hx1 hy1 hz1
    xh1 xx1 xy1 xz1
    yh1 yx1 yy1 yz1
    zh1 zx1 zy1 zz1) *> (HI
     hh2 hx2 hy2 hz2
     xh2 xx2 xy2 xz2
     yh2 yx2 yy2 yz2
     zh2 zx2 zy2 zz2) = HI {- I am sorry for this -}
      (hh1*hh2 + hx1*xh2 + hy1*yh2 + hz1*zh2)
      (hh1*hx2 + hx1*xx2 + hy1*yx2 + hz1*zx2)
      (hh1*hy2 + hx1*xy2 + hy1*yy2 + hz1*zy2)
      (hh1*hz2 + hx1*xz2 + hy1*yz2 + hz1*zz2)
      (xh1*hh2 + xx1*xh2 + xy1*yh2 + xz1*zh2)
      (xh1*hx2 + xx1*xx2 + xy1*yx2 + xz1*zx2)
      (xh1*hy2 + xx1*xy2 + xy1*yy2 + xz1*zy2)
      (xh1*hz2 + xx1*xz2 + xy1*yz2 + xz1*zz2)
      (yh1*hh2 + yx1*xh2 + yy1*yh2 + yz1*zh2)
      (yh1*hx2 + yx1*xx2 + yy1*yx2 + yz1*zx2)
      (yh1*hy2 + yx1*xy2 + yy1*yy2 + yz1*zy2)
      (yh1*hz2 + yx1*xz2 + yy1*yz2 + yz1*zz2)
      (zh1*hh2 + zx1*xh2 + zy1*yh2 + zz1*zh2)
      (zh1*hx2 + zx1*xx2 + zy1*yx2 + zz1*zx2)
      (zh1*hy2 + zx1*xy2 + zy1*yy2 + zz1*zy2)
      (zh1*hz2 + zx1*xz2 + zy1*yz2 + zz1*zz2)

instance (Num c) => Monoid (Isometry c) where
  mempty = identity
  mappend = (*>)

identity :: (Num c) => Isometry c
identity = HI
  1 0 0 0
  0 1 0 0
  0 0 1 0
  0 0 0 1

pushx alpha = let
  c = cosh alpha
  s = sinh alpha
  in HI
   c s 0 0
   s c 0 0
   0 0 1 0
   0 0 0 1

pushy alpha = let
  c = cosh alpha
  s = sinh alpha
  in HI
   c 0 s 0
   0 1 0 0
   s 0 c 0
   0 0 0 1

pushz alpha = let
  c = cosh alpha
  s = sinh alpha
  in HI
   c 0 0 s
   0 1 0 0
   0 0 1 0
   s 0 0 c

push d = case d of
  X -> pushx
  Y -> pushy
  Z -> pushz

rotateToXY :: (Num c, Floating c) =>
  HyperPoint c -> (Isometry c, Isometry c)
rotateToXY (HP h x y z) = let
  l = sqrt (x ^ 2 + z ^ 2)
  c = x / l
  s = z / l
  in (HI
    1   0  0 0
    0   c  0 s
    0   0  1 0
    0 (-s) 0 c,
   HI
    1 0 0   0
    0 c 0 (-s)
    0 0 1   0
    0 s 0   c
   )

rotateToXZ :: (Num c, Floating c) =>
  HyperPoint c -> (Isometry c, Isometry c)
rotateToXZ (HP h x y z) = let
  l = sqrt (x ^ 2 + y ^ 2)
  c = x / l
  s = y / l
  in (HI
    1   0  0 0
    0   c  s 0
    0 (-s) c 0
    0   0  0 1,
   HI
    1 0   0  0
    0 c (-s) 0
    0 s   c  0
    0 0   0  1
   )

rotateToX :: (Num c, Floating c) =>
  HyperPoint c -> (Isometry c, Isometry c)
rotateToX p = let
  (rt1,rf1) = rotateToXY p
  (rt2,rf2) = rotateToXZ (rt1 *> p)
  in (rt2 *> rt1, rf1 *> rf2)

{-
Define vertical as perpendicular to the plane at z = 0

Step 1: find the point where z = 0 with minimum distance from original point
d = acosh (h * sqrt (x'^2 + y'^2 + 1) - x*x' - y*y')
dd/dx' = 1/sqrt(h*sqrt (x'^2 + y'^2 + 1) - x*x' - y*y' - 1) *
  (2hx' / 2*sqrt (x'^2 + y'^2 + 1) - x)
dd/dy' = 1/sqrt(h*sqrt (x'^2 + y'^2 + 1) - x*x' - y*y' - 1) *
  (2hy' / 2*sqrt (x'^2 + y'^2 + 1) - y)
dd/dx' = dd/dy' = 0
0 = 2hx' /
  (sqrt(h*sqrt (x'^2 + y'^2 + 1) - x*x' - y*y' - 1) * 2*sqrt (x'^2 + y'^2 + 1) - x)
2hx' / x = 2hx' / sqrt (h * sqrt((x'^2 + y'^2 + 1) - x*x' - y*y' - 1) * (x'^2 + y'^2 + 1))
 
-}
