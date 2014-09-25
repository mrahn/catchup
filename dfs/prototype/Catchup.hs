{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Catchup (Catchup, catchup, result, mirror) where

import qualified Util (join, select, unique)
import qualified Player (Player (Blue, Orange), other)
import qualified Put (Put (put))
import qualified Point2D (Point2D)

import HexBoard (HexBoard)
import qualified HexBoard
  ( empty, free_fields, stone, depth, id_of_point2D, rotate60
  , component_sizes, size_of_components, mirror
  )
import qualified Data.Map (Map, empty, insert, lookup)
import qualified Data.Set (toList)
import qualified Control.Monad.State (State, get, modify, evalState)

------------------------------------------------------------------------------

data Catchup = Catchup { board :: HexBoard
                       , to_move :: Player.Player
                       , high_water :: Int
                       , available_stones :: [Int]
                       } deriving (Eq, Ord)

catchup :: Int -> Catchup
catchup n = Catchup { board = HexBoard.empty n
                    , to_move = Player.Blue
                    , high_water = 0
                    , available_stones = [1]
                    }

instance Show Catchup where
  showsPrec 0 c = (++) header
                . (++) "\n" . (++) (show $ HexBoard.component_sizes Player.Blue $ board c)
                . (++) "\n" . (++) (show $ HexBoard.component_sizes Player.Orange $ board c)
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
          csize = maximum $ HexBoard.size_of_components (to_move c) new_board fields

instance Put.Put [Point2D.Point2D] Catchup where
  put ps c = Put.put (map (HexBoard.id_of_point2D (board c)) ps) c

instance Put.Put [String] Catchup where
  put input = Put.put (map read input :: [Point2D.Point2D])

------------------------------------------------------------------------------

rotate60 :: Catchup -> Catchup
rotate60 c = c { board = HexBoard.rotate60 $ board c }

mirror :: Int -> Catchup -> Catchup
mirror k c = c { board = HexBoard.mirror k $ board c }

mirrors :: Catchup -> [Catchup]
mirrors c = map (flip mirror c) [0..5]

equiv :: Catchup -> [Catchup]
equiv c = mirrors c ++ mirrors (rotate60 c)

normal :: Catchup -> Catchup
normal = minimum . equiv

------------------------------------------------------------------------------

all_suc :: Catchup -> [Catchup]
all_suc c = concat [ sucN k c | k <- available_stones c ]

suc :: Catchup -> [Catchup]
suc c
  | HexBoard.depth (board c) > 5 = all_suc c
  | otherwise = Util.unique $ map normal $ all_suc c

sucN :: Int -> Catchup -> [Catchup]
sucN k c = map (flip Put.put c)
         $ Util.select k (Data.Set.toList $ HexBoard.free_fields $ board c)

paths :: Catchup -> [[Catchup]]
paths c
  | null cs = [[]]
  | otherwise = [ s:p | s <- cs, p <- paths s ]
  where cs = suc c

------------------------------------------------------------------------------

resultM :: Catchup
        -> Control.Monad.State.State
           (Data.Map.Map (Player.Player, Data.Map.Map Int Player.Player) Player.Player) Player.Player
resultM c = do
    cache <- Control.Monad.State.get
    let key =  (to_move c, HexBoard.stone $ board c)
    case Data.Map.lookup key cache of
      Just v -> return v
      Nothing -> do
        r <- case suc c of
          [] -> return $ compare_sizes
                           (HexBoard.component_sizes Player.Blue $ board c)
                           (HexBoard.component_sizes Player.Orange $ board c)
          cs -> do rs <- mapM resultM cs
                   let sel = (if any (==to_move c) rs then id else Player.other)
                   return $ sel (to_move c)
        Control.Monad.State.modify (Data.Map.insert key r)
        return r

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
