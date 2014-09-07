{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import qualified Data.Bits (shiftL, (.&.), (.|.))
import qualified Data.Char (chr, ord)
import qualified Data.Int (Int64)
import qualified Data.Map
   (Map, empty, insert, insertWith, lookup, (!), fromList, findWithDefault)
import qualified Data.Set (Set, fromList, toList, delete)
import qualified Data.List (groupBy, intersperse, sortBy)
import qualified Control.Monad.State (State, get, modify, evalState)

------------------------------------------------------------------------------

join :: [a] -> [[a]] -> [a]
join sep = concat . Data.List.intersperse sep

select :: Int -> [a] -> [[a]]
select 0 _ = [[]]
select _ [] = []
select n (x:xs) = [ (x:ys) | ys <- select (pred n) xs ] ++ select n xs

------------------------------------------------------------------------------

data HexPoint = HexPoint Int Int Int deriving (Eq, Ord)

hex_points :: Int -> [HexPoint]
hex_points n = [ HexPoint x y z | x <- line, y <- line, z <- line
                                , x + y + z == 0
               ]
  where line = [negate (pred n) .. pred n]

-- num_hex_points n == length (hex_points n)
num_hex_points :: Int -> Int
num_hex_points n = 3 * n * (n - 1) + min 1 n

hex_fields :: Int -> [Int]
hex_fields n = [0.. pred (num_hex_points n)]

point_of_id :: Int -> (Int -> HexPoint)
point_of_id n = (Data.Map.!) (Data.Map.fromList $ zip [0..] $ hex_points n)

id_of_point :: Int -> (HexPoint -> Int)
id_of_point n = (Data.Map.!) (Data.Map.fromList $ zip (hex_points n) [0..])

coordinates :: HexPoint -> [Int]
coordinates (HexPoint x y z) = [x, y, z]

distance :: HexPoint -> HexPoint -> Int
distance p =
  flip div 2 . sum . map abs . zipWith (-) (coordinates p) . coordinates

neighbouring :: Int -> [(HexPoint, [HexPoint])]
neighbouring n =
  [ (p, [ q | q <- hex_points n, distance q p == 1 ]) | p <- hex_points n ]

------------------------------------------------------------------------------

data Player = Blue | Orange deriving (Eq, Ord)

other :: Player -> Player
other Blue = Orange
other Orange = Blue

instance Show Player where
  showsPrec _ Blue = (++) "B"
  showsPrec _ Orange = (++) "O"

------------------------------------------------------------------------------

data HexBoard = HexBoard { size :: Int
                         , stone :: Data.Map.Map Int Player
                         , taken :: Data.Map.Map Player [Int]
                         , id_of_point2D :: Point2D -> Int
                         , neighbours :: Int -> [Int]
                         , free_fields :: Data.Set.Set Int
                         }

instance Eq HexBoard where x == y = stone x == stone y
instance Ord HexBoard where  compare x y = compare (stone x) (stone y)

instance Put (Player, Int) HexBoard where
  put (player, field) b =
    b { stone = Data.Map.insert field player (stone b)
      , taken = Data.Map.insertWith (++) player [field] (taken b)
      , free_fields = Data.Set.delete field (free_fields b)
      }

empty_hex_board :: Int -> HexBoard
empty_hex_board n | length (hex_fields n) > 64 =
  error "Size to large. Traversal state has 64 bits only. Sorry."
empty_hex_board n = HexBoard
  { size = n
  , stone = Data.Map.empty
  , taken = Data.Map.empty
  , id_of_point2D = id_of_point n . hexangular n
  , neighbours = (Data.Map.!)
      (Data.Map.fromList $ map (ap (id_of_point n)) $ neighbouring n)
  , free_fields = Data.Set.fromList $ hex_fields n
  }
  where ap f (x, y) = (f x, map f y)

------------------------------------------------------------------------------

data Point2D = Point2D Int Int

instance Show Point2D where
  showsPrec _ (Point2D x y) = (:) (Data.Char.chr (x + 97)) . (++) (show y)

instance Read Point2D where
  readsPrec _ (x:xs) = [(Point2D (Data.Char.ord x - 97) (read xs), [])]

rectangluar :: Int -> HexPoint -> Point2D
rectangluar n (HexPoint x y _) = Point2D (pred n + x) (pred n + y + min 0 x)

hexangular :: Int -> Point2D -> HexPoint
hexangular n (Point2D x y) = HexPoint hx hy hz
  where hx = x - pred n
        hy = y - pred n - min 0 hx
        hz = -(hx + hy)

------------------------------------------------------------------------------

class Put a b where put :: a -> b -> b

------------------------------------------------------------------------------

instance Put (Player, Point2D) HexBoard where
  put (player, p) b = put (player, id_of_point2D b p) b

------------------------------------------------------------------------------

rows :: HexBoard -> [[Int]]
rows b = Data.List.groupBy same_x (hex_fields n)
  where n = size b
        coordinate_x f = let Point2D x _ = rectangluar n $ point_of_id n f
                         in x
        same_x f1 f2 = coordinate_x f1 == coordinate_x f2

instance Show HexBoard where
  showsPrec 0 b = (++) (join "\n" $ map nice_row $ rows b)
    where nice_field field = case Data.Map.lookup field (stone b) of
                               Nothing -> "."
                               Just player -> show player
          nice_row row = prefix (length row) ++ join " " (map nice_field row)
          prefix k = replicate (2 * size b - 1 - k) ' '

------------------------------------------------------------------------------

eval_int64 :: Control.Monad.State.State Data.Int.Int64 a -> a
eval_int64 = flip Control.Monad.State.evalState 0

componentM :: Player -> HexBoard -> Int
           -> Control.Monad.State.State Data.Int.Int64 Int
componentM player b field
  | Data.Map.lookup field (stone b) == Just player = do
    cache <- Control.Monad.State.get
    let pos = Data.Bits.shiftL 1 field
    case (pos Data.Bits..&. cache /= 0) of
      False -> do Control.Monad.State.modify ((Data.Bits..|.) pos)
                  vs <- mapM (componentM player b) (neighbours b field)
                  return $ succ $ sum vs
      _ -> return 0
componentM _ _ _ = return 0

componentsM :: Player -> HexBoard -> [Int]
            -> Control.Monad.State.State Data.Int.Int64 [Int]
componentsM player b = mapM (componentM player b)

size_of_components :: Player -> HexBoard -> [Int] -> [Int]
size_of_components player b fs = eval_int64 (componentsM player b fs)

all_componentsM :: Player -> HexBoard
                -> Control.Monad.State.State Data.Int.Int64 [Int]
all_componentsM player b =
  componentsM player b (Data.Map.findWithDefault [] player (taken b))

component_sizes :: Player -> HexBoard -> [Int]
component_sizes player b = Data.List.sortBy (flip compare)
                         $ strip $ eval_int64 (all_componentsM player b)
  where strip = filter (>0)

------------------------------------------------------------------------------

data Catchup = Catchup { board :: HexBoard
                       , to_move :: Player
                       , high_water :: Int
                       , available_stones :: [Int]
                       } deriving (Eq, Ord)

catchup :: Int -> Catchup
catchup n = Catchup { board = empty_hex_board n
                    , to_move = Blue
                    , high_water = 0
                    , available_stones = [1]
                    }

instance Show Catchup where
  showsPrec 0 c = (++) header
                . (++) "\n" . (++) (show $ component_sizes Blue $ board c)
                . (++) "\n" . (++) (show $ component_sizes Orange $ board c)
                . (++) "\n" . showsPrec 0 (board c)
    where header = join ", " [ show (high_water c)
                             , show (to_move c)
                             , show (available_stones c)
                             ]

instance Put [Int] Catchup where
  put fields c = Catchup { board = new_board
                         , to_move = other (to_move c)
                         , high_water = max csize $ high_water c
                         , available_stones =
                             if high_water c > 0 && csize > high_water c
                             then [1,2,3] else [1,2]
                         }
    where new_board = puts fields (board c)
          puts (f:fs) b = puts fs (put (to_move c, f) b)
          puts [] b = b
          csize = maximum $ size_of_components (to_move c) new_board fields

instance Put [Point2D] Catchup where
  put ps c = put (map (id_of_point2D (board c)) ps) c

instance Put [String] Catchup where
  put input = put (map read input :: [Point2D])

------------------------------------------------------------------------------

suc :: Catchup -> [Catchup]
suc c = concat [ sucN k c | k <- available_stones c ]

sucN :: Int -> Catchup -> [Catchup]
sucN k c = map (flip put c)
         $ select k (Data.Set.toList $ free_fields $ board c)

paths :: Catchup -> [[Catchup]]
paths c
  | null cs = [[]]
  | otherwise = [ s:p | s <- cs, p <- paths s ]
  where cs = suc c

------------------------------------------------------------------------------

resultM :: Catchup
        -> Control.Monad.State.State (Data.Map.Map Catchup Player) Player
resultM c
  | null cs = return $ compare_sizes (component_sizes Blue $ board c)
                                     (component_sizes Orange $ board c)
  | otherwise = do cache <- Control.Monad.State.get
                   case Data.Map.lookup c cache of
                     Just v -> return v
                     Nothing -> do
                       rs <- mapM resultM cs
                       let sel = (if any (==to_move c) rs then id else other)
                           r = sel (to_move c)
                       Control.Monad.State.modify (Data.Map.insert c r)
                       return r
  where cs = suc c

compare_sizes :: [Int] -> [Int] -> Player
compare_sizes (b:bs) (o:os) = case compare b o of
                                GT -> Blue
                                LT -> Orange
                                EQ -> compare_sizes bs os
compare_sizes (_:_) [] = Blue
compare_sizes _ _ = Orange

eval_map :: Control.Monad.State.State (Data.Map.Map k v) a -> a
eval_map = flip Control.Monad.State.evalState Data.Map.empty

result :: Catchup -> Player
result c = eval_map (resultM c)

------------------------------------------------------------------------------

main :: IO ()
main = print $ result $ lg1657875

------------------------------------------------------------------------------

read_lg_move :: Int -> String -> Point2D
read_lg_move n (c:xs) = Point2D x (shift y)
  where x = (Data.Char.ord c - 65)
        y = read xs - 1
        shift = (+) (min 0 (x - pred n))

import_lg_game :: [[String]] -> Catchup
import_lg_game = foldl (flip put) (catchup 5) . map (map (read_lg_move 5))

lg1657875 :: Catchup
lg1657875 = import_lg_game moves
  where moves = [ ["E5"]
                , ["F4","C6"]
                , ["D4","F6"]
                , ["E6","E3"]
                , ["D7","H5"]
                , ["F5","D6"]
                , ["E7","C7","B7"]
                , ["G5","G6"]
                , ["F7","G7","H6"]
                , ["B6","A7","A8"]
                , ["H4","G3","H2"]
                , ["F3","G2"]
                , ["H1","G1","E2"]
                , ["G4","H3"]
                , ["I2","I3","D3"]
                , ["F2","F1","B8"]
                , ["C8","B9","D5"]
                , ["C4","B4"]
                , ["C3","E1"]
                , ["D8","C9"]
                , ["C5","E4"]
                , ["B5","A9"]
                ]
