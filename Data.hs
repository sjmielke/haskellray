module Data where

data Vec3 = Vec3 Double Double Double deriving Eq
type ImgArray = [[Vec3]]
data Ray = Ray {origin :: Vec3, direction :: Vec3} deriving Eq
data Sphere = Sphere {center :: Vec3, radius :: Double} deriving Eq
data Hit = Hit {ray :: Ray, object :: Sphere, lambda :: Double} deriving Eq
data Light = PointLight {position :: Vec3, color :: Vec3}
				| OrbLight {orbposition :: Vec3, orbradius :: Double, color :: Vec3}

instance Num (Vec3) where
	(Vec3 x y z) + (Vec3 a b c) = (Vec3 (x+a) (y+b) (z+c))
	(Vec3 x y z) * (Vec3 a b c) = (Vec3 (x*a) (y*b) (z*c))
	(Vec3 x y z) - (Vec3 a b c) = (Vec3 (x-a) (y-b) (z-c))
	abs    (Vec3 x y z) = (Vec3 (abs x) (abs y) (abs z)) 
	signum (Vec3 x y z) = (Vec3 (signum x) (signum y) (signum z))
	fromInteger i = (Vec3 (fromIntegral i) (fromIntegral i) (fromIntegral i))

instance Ord (Hit) where
	compare (Hit _ _ l1) (Hit _ _ l2) = compare l1 l2
