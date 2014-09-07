{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import qualified Util (join, select, unique)
import qualified HexPoint
  (fields, id_of_point, point_of_id, neighbouring, rotate60, rotate300)
import qualified Player (Player (Blue, Orange), other)
import qualified Put (Put (put))
import qualified Point2D
  (Point2D, rectangluar, hexangular, coordinate_x, read_lg_move)

import qualified Data.Bits (shiftL, (.&.), (.|.))
import qualified Data.Int (Int64)
import qualified Data.Map
   ( Map, empty, insert, insertWith, lookup, (!), fromList, findWithDefault
   , map, mapKeys
   )
import qualified Data.Set (Set, fromList, toList, delete, map)
import qualified Data.List (groupBy, sortBy)
import qualified Control.Monad.State (State, get, modify, evalState)

------------------------------------------------------------------------------

data HexBoard = HexBoard { size :: Int
                         , stone :: Data.Map.Map Int Player.Player
                         , taken :: Data.Map.Map Player.Player [Int]
                         , id_of_point2D :: Point2D.Point2D -> Int
                         , neighbours :: Int -> [Int]
                         , free_fields :: Data.Set.Set Int
                         }

instance Eq HexBoard where x == y = stone x == stone y
instance Ord HexBoard where  compare x y = compare (stone x) (stone y)

instance Put.Put (Player.Player, Int) HexBoard where
  put (player, field) b =
    b { stone = Data.Map.insert field player (stone b)
      , taken = Data.Map.insertWith (++) player [field] (taken b)
      , free_fields = Data.Set.delete field (free_fields b)
      }

empty_hex_board :: Int -> HexBoard
empty_hex_board n | length (HexPoint.fields n) > 64 =
  error "Size to large. Traversal state has 64 bits only. Sorry."
empty_hex_board n = HexBoard
  { size = n
  , stone = Data.Map.empty
  , taken = Data.Map.empty
  , id_of_point2D = HexPoint.id_of_point n . Point2D.hexangular n
  , neighbours = (Data.Map.!)
      (Data.Map.fromList $ map (ap (HexPoint.id_of_point n)) $ HexPoint.neighbouring n)
  , free_fields = Data.Set.fromList $ HexPoint.fields n
  }
  where ap f (x, y) = (f x, map f y)

------------------------------------------------------------------------------

hex_board_rotate60 b = b
  { stone = Data.Map.mapKeys rot60 $ stone b
  , taken = Data.Map.map (map rot60) $ taken b
  , id_of_point2D = rot60 . id_of_point2D b
  , neighbours = map rot60 . neighbours b . rot300
  , free_fields = Data.Set.map rot60 $ free_fields b
  }
  where n = size b
        rot60 = HexPoint.id_of_point n . HexPoint.rotate60 . HexPoint.point_of_id n
        rot300 = HexPoint.id_of_point n . HexPoint.rotate300 . HexPoint.point_of_id n

------------------------------------------------------------------------------

instance Put.Put (Player.Player, Point2D.Point2D) HexBoard where
  put (player, p) b = Put.put (player, id_of_point2D b p) b

------------------------------------------------------------------------------

rows :: HexBoard -> [[Int]]
rows b = Data.List.groupBy same_x (HexPoint.fields n)
  where n = size b
        coordinate_x f = Point2D.coordinate_x $ Point2D.rectangluar n $ HexPoint.point_of_id n f
        same_x f1 f2 = coordinate_x f1 == coordinate_x f2

instance Show HexBoard where
  showsPrec 0 b = (++) (Util.join "\n" $ map nice_row $ rows b)
    where nice_field field = case Data.Map.lookup field (stone b) of
                               Nothing -> "."
                               Just player -> show player
          nice_row row = prefix (length row) ++ Util.join " " (map nice_field row)
          prefix k = replicate (2 * size b - 1 - k) ' '

------------------------------------------------------------------------------

eval_int64 :: Control.Monad.State.State Data.Int.Int64 a -> a
eval_int64 = flip Control.Monad.State.evalState 0

componentM :: Player.Player -> HexBoard -> Int
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

componentsM :: Player.Player -> HexBoard -> [Int]
            -> Control.Monad.State.State Data.Int.Int64 [Int]
componentsM player b = mapM (componentM player b)

size_of_components :: Player.Player -> HexBoard -> [Int] -> [Int]
size_of_components player b fs = eval_int64 (componentsM player b fs)

all_componentsM :: Player.Player -> HexBoard
                -> Control.Monad.State.State Data.Int.Int64 [Int]
all_componentsM player b =
  componentsM player b (Data.Map.findWithDefault [] player (taken b))

component_sizes :: Player.Player -> HexBoard -> [Int]
component_sizes player b = Data.List.sortBy (flip compare)
                         $ strip $ eval_int64 (all_componentsM player b)
  where strip = filter (>0)

------------------------------------------------------------------------------

data Catchup = Catchup { board :: HexBoard
                       , to_move :: Player.Player
                       , high_water :: Int
                       , available_stones :: [Int]
                       } deriving (Eq, Ord)

catchup :: Int -> Catchup
catchup n = Catchup { board = empty_hex_board n
                    , to_move = Player.Blue
                    , high_water = 0
                    , available_stones = [1]
                    }

instance Show Catchup where
  showsPrec 0 c = (++) header
                . (++) "\n" . (++) (show $ component_sizes Player.Blue $ board c)
                . (++) "\n" . (++) (show $ component_sizes Player.Orange $ board c)
                . (++) "\n" . showsPrec 0 (board c)
    where header = Util.join ", " [ show (high_water c)
                                  , show (to_move c)
                                  , show (available_stones c)
                                  ]

instance Put.Put [Int] Catchup where
  put fields c = Catchup { board = new_board
                         , to_move = Player.other (to_move c)
                         , high_water = max csize $ high_water c
                         , available_stones =
                             if high_water c > 0 && csize > high_water c
                             then [1,2,3] else [1,2]
                         }
    where new_board = puts fields (board c)
          puts (f:fs) b = puts fs (Put.put (to_move c, f) b)
          puts [] b = b
          csize = maximum $ size_of_components (to_move c) new_board fields

instance Put.Put [Point2D.Point2D] Catchup where
  put ps c = Put.put (map (id_of_point2D (board c)) ps) c

instance Put.Put [String] Catchup where
  put input = Put.put (map read input :: [Point2D.Point2D])

------------------------------------------------------------------------------

catchup_rotate60 :: Catchup -> Catchup
catchup_rotate60 c = c { board = hex_board_rotate60 $ board c }

rotations :: Catchup -> [Catchup]
rotations = take 6 . iterate catchup_rotate60

normal :: Catchup -> Catchup
normal = minimum . rotations

------------------------------------------------------------------------------

suc :: Catchup -> [Catchup]
suc c = Util.unique $ map normal $ concat [ sucN k c | k <- available_stones c ]

sucN :: Int -> Catchup -> [Catchup]
sucN k c = map (flip Put.put c)
         $ Util.select k (Data.Set.toList $ free_fields $ board c)

paths :: Catchup -> [[Catchup]]
paths c
  | null cs = [[]]
  | otherwise = [ s:p | s <- cs, p <- paths s ]
  where cs = suc c

------------------------------------------------------------------------------

resultM :: Catchup
        -> Control.Monad.State.State
           (Data.Map.Map (Player.Player, Data.Map.Map Int Player.Player) Player.Player) Player.Player
resultM c
  | null cs = return $ compare_sizes (component_sizes Player.Blue $ board c)
                                     (component_sizes Player.Orange $ board c)
  | otherwise = do cache <- Control.Monad.State.get
                   let key = (to_move c, stone $ board c)
                   case Data.Map.lookup key cache of
                     Just v -> return v
                     Nothing -> do
                       rs <- mapM resultM cs
                       let sel = (if any (==to_move c) rs then id else Player.other)
                           r = sel (to_move c)
                       Control.Monad.State.modify (Data.Map.insert key r)
                       return r
  where cs = suc c

compare_sizes :: [Int] -> [Int] -> Player.Player
compare_sizes (b:bs) (o:os) = case compare b o of
                                GT -> Player.Blue
                                LT -> Player.Orange
                                EQ -> compare_sizes bs os
compare_sizes (_:_) [] = Player.Blue
compare_sizes _ _ = Player.Orange

eval_map :: Control.Monad.State.State (Data.Map.Map k v) a -> a
eval_map = flip Control.Monad.State.evalState Data.Map.empty

result :: Catchup -> Player.Player
result c = eval_map (resultM c)

------------------------------------------------------------------------------

main :: IO ()
main = print $ result $ lg1657875

------------------------------------------------------------------------------

import_lg_game :: [[String]] -> Catchup
import_lg_game = foldl (flip Put.put) (catchup 5) . map (map (Point2D.read_lg_move 5))

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
