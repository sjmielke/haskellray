import Math
import Data

-- Advanced Trace/Math functions.

evalLambda :: Ray -> Double -> Vec3
evalLambda (Ray origin dir) lambda = origin + (scale lambda dir)

sphereCuts :: Ray -> Sphere -> [Double]
sphereCuts (Ray o d) (Sphere s radius) = solveQuadratic (sqrlength d) (2 * (dot d $ o-s)) (sqrlength (o-s) - radius*radius)

anySphereCut :: Sphere -> Ray -> Sphere -> Bool
anySphereCut nohit ray sphere = checkInLambda $ sphereCuts ray sphere
	where
		checkInLambda [] = False
		checkInLambda (s:sl) = (s > 0.01 && nohit /= sphere) || checkInLambda sl

nearestSphereCut :: Ray -> Sphere -> Maybe Double
nearestSphereCut r s = if (sphereCuts r s) == [] then Nothing else Just (minimum . filter (>0) $ sphereCuts r s)

-- PPM image file format functions and tests.

printVectorForPPM :: Vec3 -> String
printVectorForPPM (Vec3 a b c) = concat [show . round $ a, " ", show . round $ b, " ", show . round $ c, " "]

printPPM :: ImgArray -> String
printPPM [] = ""
printPPM [[]] = ""
printPPM arr = "P3 " ++ (show . length . head $ arr) ++ " " ++ (show . length $ arr) ++ " 255 " ++ (concat . map printVectorForPPM . concat $ arr)

renderSimpleFunc :: ((Int, Int) -> Vec3) -> ImgArray
renderSimpleFunc renderFunc = [[renderFunc (x,y) | x <- [0..(picSize-1)]] | y <- [0..(picSize-1)]]

traceRayForPixel :: (Int, Int) -> Vec3
traceRayForPixel (x,y) = if length hitlist == 0 then fromInteger 0 else getShading (minimum hitlist) objects lights
	where
		hitlist = concat . map (traceRayOnObject ray) $ objects
		ray = Ray camera $ normvec (Vec3 (fromIntegral x-(picSize / 2)) (fromIntegral y-(picSize / 2)) (6*picSize))
		camera = (Vec3 0 0 (-20))
		getlambda (Hit _ _ l) = l
		lights = [PointLight (Vec3 4 0 (-18)) (Vec3 255 255 255)]
		objects = threespheres
		twospheres = [(Sphere (Vec3 (-1) 0 0.4) 1.5), (Sphere (Vec3 1 0 0.4) 1.5)]
		threespheres = [(Sphere (Vec3 (-0.6) 0 5) 0.7), (Sphere (Vec3 0 0 0) 0.3), (Sphere (Vec3 1 0 (-3)) 0.3)]

getShading :: Hit -> [Sphere]-> [Light] -> Vec3
getShading hit objects = sum . map (getShadingPerLight hit objects)

getShadingPerLight :: Hit -> [Sphere] -> Light -> Vec3
--getShadingPerLight hit objects (OrbLight orbpos ordradius orbint) = getShadingPerLight hit objects (PointLight (scale ordradius . normvec $ randomvec) lightint)
getShadingPerLight (Hit ray@(Ray ori dir) sphere@(Sphere center radius) lambda) objects (PointLight lightpos lightint) =
	if foldl (||) False $ map (anySphereCut sphere (Ray sectPoint (lightpos - sectPoint))) objects then Vec3 0 0 0 else
		(scale (poso $ dot w_in normal) (lightint * r_diffuse)) +
		(if (dot w_in normal) < 0 then Vec3 0 0 0 else scale ((poso $ dot w_half normal)**shininess) (lightint * r_specular))
	where
		r_diffuse = Vec3 0.1 0.1 0.5
		r_specular = Vec3 0.3 0.2 0.2
		shininess = 20.0
		w_in = normvec $ lightpos - sectPoint
		w_out = normvec $ scale (-1) dir
		w_half = normvec $ w_in + w_out
		normal = normvec $ sectPoint - center
		sectPoint = evalLambda ray lambda

traceRayOnObject :: Ray -> Sphere -> [Hit]
traceRayOnObject ray obj = case nearestSphereCut ray obj of
							Nothing -> []
							Just d -> [Hit ray obj d]

picSize :: (Num a) => a
picSize = 512

main = do
	putStr . printPPM $ renderSimpleFunc traceRayForPixel


{- Old stuff.

makeGradient :: ImgArray
makeGradient = map ((replicate picSize . fromInteger . fromIntegral) . (`div` 2)) $ [(0::Int)..(picSize-1)]

defineRect :: (Int, Int) -> Vec3
defineRect (x,y) = if x >= x1 && x <= x2 && y >= y1 && y <= y2 then (fromInteger 255) else (fromInteger 0)
	where x1 = 100; x2 = 400; y1 = 100; y2 = 400

defineCircle :: (Int, Int) -> Vec3
defineCircle (x,y) = if (fromIntegral x-256)**2 + (fromIntegral y-256)**2 < 200**2 then (fromInteger 255) else (fromInteger 0)

-}