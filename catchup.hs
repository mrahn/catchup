{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import qualified Util (join, select, unique)
import qualified Player (Player (Blue, Orange), other)
import qualified Put (Put (put))
import qualified Point2D (Point2D, read_lg_move)

import HexBoard (HexBoard)
import qualified HexBoard
  ( empty, free_fields, stone, id_of_point2D, rotate60
  , component_sizes, size_of_components
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

catchup_rotate60 :: Catchup -> Catchup
catchup_rotate60 c = c { board = HexBoard.rotate60 $ board c }

rotations :: Catchup -> [Catchup]
rotations = take 6 . iterate catchup_rotate60

normal :: Catchup -> Catchup
normal = minimum . rotations

------------------------------------------------------------------------------

suc :: Catchup -> [Catchup]
suc c = Util.unique $ map normal $ concat [ sucN k c | k <- available_stones c ]

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
resultM c
  | null cs = return $ compare_sizes (HexBoard.component_sizes Player.Blue $ board c)
                                     (HexBoard.component_sizes Player.Orange $ board c)
  | otherwise = do cache <- Control.Monad.State.get
                   let key = (to_move c, HexBoard.stone $ board c)
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
