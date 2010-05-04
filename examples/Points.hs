{-# OPTIONS_GHC -fglasgow-exts #-}

import Data.Has
import Control.Applicative
import Test.QuickCheck

data X = X; type instance TypeOf X = Int
data Y = Y; type instance TypeOf Y = Int
data Z = Z; type instance TypeOf Z = Int

type Point2D = FieldOf X :&: FieldOf Y

getXY :: (Has X p, Has Y p) => p -> (Int,Int)
getXY = liftA2 (,) (X ^.) (Y ^.)

p2 :: Int -> Int -> Point2D
p2 x y = X .> x & Y .> y

dist2d :: (Has X p, Has Y p) => p -> p -> Double
dist2d p1 p2 = let (x1,y1) = getXY p1; (x2,y2) = getXY p2
               in sqrt . fromIntegral $ ((x2-x1)^2) + ((y2-y1)^2)

p = p2 1 3
q = p2 4 5
r = p2 8 (-2)

d0 = dist2d p q
d1 = dist2d p r

type Point3D = FieldOf X :&: FieldOf Y :&: FieldOf Z

getXYZ :: (Has X p, Has Y p, Has Z p)
       => p -> (Int,Int,Int)
getXYZ = liftA3 (,,) (X ^.) (Y ^.) (Z ^.)

p3 :: Int -> Int -> Int -> Point3D
p3 x y z = X .> x & Y .> y & Z .> z

dist3d :: (Has X p, Has Y p, Has Z p)
       => p -> p -> Double
dist3d p1 p2 = let (x1,y1,z1) = getXYZ p1; (x2,y2,z2) = getXYZ p2
               in sqrt . fromIntegral $ ((x2-x1)^2) + ((y2-y1)^2) + ((z2-z1)^2)

p' = p3 1 1 0
q' = p3 3 4 2
r' = p3 6 (-1) 2

d3 = dist3d p' q'
d4 = dist3d p' r'

-- dist2d still can be applied to Point3D
-- Z value is simply ignored
d3' = dist2d p' q'
d4' = dist2d p' r'

-- Define that property for points
-- check :: Point3D -> Point3D -> Bool
check p q = dist2d p q === dist3d (ignoreZ p) (ignoreZ q)
  where
    x === y | isNaN x && isNaN y = True
            | otherwise          = x == y

ignoreZ :: (Has Z p) => p -> p
ignoreZ = Z ^= 0

-- Time to quickCheck it.
-- Note Arbitrary instance is provided automatically
-- (And the place of signiture to avoid weird error)
main = quickCheck (check :: Point3D -> Point3D -> Bool)
