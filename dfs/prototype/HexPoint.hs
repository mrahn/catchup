module HexPoint
  ( HexPoint (HexPoint), fields, id_of_point, point_of_id, neighbouring
  , rotate60, rotate300, mirror, rorrim
  )
where

import qualified Data.Map (fromList, (!), fromListWith, Map)
import Data.List (sort, sortBy, intersperse)
import qualified Data.Set

uniq :: Ord a => [a] -> [a]
uniq = Data.Set.toList . Data.Set.fromList

uniqBy :: (Ord b, Eq b) => (a -> b) -> [a] -> [(a, b)]
uniqBy f = dedup . sortBy (\ x y -> compare (snd x) (snd y)) . decorate f
  where dedup (x:y:rest)
          | snd x == snd y = dedup (x:rest)
          | otherwise = x : dedup (y:rest)
        dedup rest = rest

decorate f xs = [ (x, f x) | x <- xs ]

------------------------------------------------------------------------------

data HexPoint = HexPoint Int Int Int deriving (Eq, Ord, Show, Read)

grow (HexPoint x y z) =
   [ HexPoint (x + 1)  y      (z - 1)
   , HexPoint (x - 1)  y      (z + 1)
   , HexPoint (x + 1) (y - 1)  z
   , HexPoint (x - 1) (y + 1)  z
   , HexPoint  x      (y + 1) (z - 1)
   , HexPoint  x      (y - 1) (z + 1)
   ]

select [] = []
select (x:xs) = (x,xs) : [ (y,x:rest) | (y,rest) <- select xs ]

grows ps = [ (p:s:rest) | (p,rest) <- select ps, s <- grow p ]

growS b = filter (\ n -> length n > length b) . map uniq . grows $ b

boards 0 = [[HexPoint 0 0 0]]
boards n = uniq . map normal . concatMap growS . boards . pred $ n

normal hs = minimum [ Data.List.sort ns | t <- ts, let ns = map t hs ]

permutations :: [a] -> [[a]]
permutations []     = [[]]
permutations (x:xs) = [ zs | ys <- permutations xs , zs <- everywhere x ys ]

everywhere :: a -> [a] -> [[a]]
everywhere x []     = [[x]]
everywhere x (y:ys) = (x:y:ys) : [ y:zs | zs <- everywhere x ys ]

cross l = [ (x,y) | x <- l, y <- l ]

mk_s i b = let hs = zip i b
           in [ (p,q) | ((p,l),(q,r)) <- cross hs, neighbourQ l r ]

struct :: [HexPoint] -> [(Int, Int)]
struct b =
  minimum $ map (sort . flip mk_s b) (permutations $ take (length b) [0..])

dboards :: Int -> [([HexPoint],[(Int,Int)])]
dboards = uniqBy struct . boards

mk_m :: [(Int,Int)] -> Data.Map.Map Int [Int]
mk_m = Data.Map.fromListWith (++) . map (\(x,y) -> (x,[y]))

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
distance (HexPoint a b c) (HexPoint x y z) =
  flip div 2 $ abs (a - x) + abs (b - y) + abs (c - z)

neighbourQ p = (==1) . distance p

neighbouring :: Int -> [(HexPoint, [HexPoint])]
neighbouring n =
  [ (p, [ q | q <- points n, distance q p == 1 ]) | p <- points n ]

linear :: Int -> ([Int],[Int])
linear s = let ns = map (map (id_of_point s) . snd) (neighbouring s)
           in (scanl (+) 0 $ map length ns, concat ns)

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

ts = [ mirror k | k <- [0..5] ] ++ [ mirror k . rotate60 | k <- [0..5] ]

transformations :: Int -> [[Int]]
transformations s =
  let n = num_points s
      apply t = map (id_of_point s . t) (points s)
  in map apply [ mirror k | k <- [0..5] ]
     ++ map apply [ mirror k . rotate60 | k <- [0..5] ]

trans_c :: Int -> [Int]
trans_c = concat . tail . transformations
