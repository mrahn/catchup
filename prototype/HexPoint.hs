module HexPoint
  ( HexPoint (HexPoint), fields, id_of_point, point_of_id, neighbouring
  , rotate60, rotate300, mirror, rorrim
  )
where

import qualified Data.Map (fromList, (!))

------------------------------------------------------------------------------

data HexPoint = HexPoint Int Int Int deriving (Eq, Ord)

points :: Int -> [HexPoint]
points n = [ HexPoint x y z | x <- line, y <- line, z <- line
                            , x + y + z == 0
           ]
  where line = [negate (pred n) .. pred n]

-- num_hex_points n == length (hex_points n)
num_points :: Int -> Int
num_points n = 3 * n * (n - 1) + min 1 n

fields :: Int -> [Int]
fields n = [0.. pred (num_points n)]

point_of_id :: Int -> (Int -> HexPoint)
point_of_id n = (Data.Map.!) (Data.Map.fromList $ zip [0..] $ points n)

id_of_point :: Int -> (HexPoint -> Int)
id_of_point n = (Data.Map.!) (Data.Map.fromList $ zip (points n) [0..])

coordinates :: HexPoint -> [Int]
coordinates (HexPoint x y z) = [x, y, z]

distance :: HexPoint -> HexPoint -> Int
distance p =
  flip div 2 . sum . map abs . zipWith (-) (coordinates p) . coordinates

neighbouring :: Int -> [(HexPoint, [HexPoint])]
neighbouring n =
  [ (p, [ q | q <- points n, distance q p == 1 ]) | p <- points n ]

------------------------------------------------------------------------------

app :: (Int -> Int) -> HexPoint -> HexPoint
app f (HexPoint a b c) = HexPoint (f a) (f b) (f c)

rotate60 :: HexPoint -> HexPoint
rotate60 (HexPoint x y z) = app (flip div 3)
  $ HexPoint (2*x - y + 2*z) (2*x + 2*y - z) (-x + 2*y + 2*z)

rotate300 :: HexPoint -> HexPoint
rotate300 (HexPoint x y z) = app (flip div 3)
  $ HexPoint (2*x + 2*y - z) (-x + 2*y + 2*z) (2*x - y + 2*z)

------------------------------------------------------------------------------

mirror :: Int -> HexPoint -> HexPoint
mirror 0 (HexPoint x y z) = HexPoint x y z
mirror 1 (HexPoint x y z) = HexPoint x z y
mirror 2 (HexPoint x y z) = HexPoint y x z
mirror 3 (HexPoint x y z) = HexPoint y z x
mirror 4 (HexPoint x y z) = HexPoint z x y
mirror 5 (HexPoint x y z) = HexPoint z y x

rorrim :: Int -> HexPoint -> HexPoint
rorrim 0 (HexPoint x y z) = HexPoint x y z
rorrim 1 (HexPoint x z y) = HexPoint x y z
rorrim 2 (HexPoint y x z) = HexPoint x y z
rorrim 3 (HexPoint y z x) = HexPoint x y z
rorrim 4 (HexPoint z x y) = HexPoint x y z
rorrim 5 (HexPoint z y x) = HexPoint x y z
