module Math where

import Data

-- Math function arsenal.

poso :: (Real a) => a -> a
poso n = if n>0 then n else 0

scale :: Double -> Vec3 -> Vec3
scale s (Vec3 x y z) = Vec3 (s*x) (s*y) (s*z)

normvec :: Vec3 -> Vec3
normvec (Vec3 x y z) = Vec3 (x/len) (y/len) (z/len)
	where len = sqrt (x*x+y*y+z*z)

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x y z) (Vec3 a b c) = x*a + y*b + z*c

sqrlength :: Vec3 -> Double
sqrlength (Vec3 x y z) = x*x + y*y + z*z

solveQuadratic :: (RealFloat a) => a -> a -> a -> [a] -- ax^2 + bx + c = 0
solveQuadratic a b c
	| a == 0 = if b == 0 then []
	                     else [-c/b]
	| otherwise = let d = b*b - 4*a*c in
	              if d < 0 then []
	                       else let q = (sqrt d) / (2 * a) in
	                            if q == 0 then [(-b/(2 * a))]
	                                      else [(-b/(2 * a)) + q, (-b/(2 * a)) - q]
