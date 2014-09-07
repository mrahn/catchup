{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module HexBoard
  ( HexBoard, empty, free_fields, stone, id_of_point2D, rotate60
  , component_sizes, size_of_components, mirror
  )
where

import qualified Util (join)
import qualified HexPoint
  ( fields, id_of_point, point_of_id, neighbouring, rotate60, rotate300
  , mirror, rorrim
  )
import qualified Player (Player)
import qualified Put (Put (put))
import qualified Point2D (Point2D, rectangluar, hexangular, coordinate_x)

import qualified Data.Bits (shiftL, (.&.), (.|.))
import qualified Data.Int (Int64)
import qualified Data.Map
   ( Map, empty, insert, insertWith, lookup, (!), fromList, findWithDefault
   , map, mapKeys
   )
import qualified Data.Set (Set, fromList, delete, map)
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

empty :: Int -> HexBoard
empty n | length (HexPoint.fields n) > 64 =
  error "Size to large. Traversal state has 64 bits only. Sorry."
empty n = HexBoard
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

rotate60 :: HexBoard -> HexBoard
rotate60 b = b
  { stone = Data.Map.mapKeys rot60 $ stone b
  , taken = Data.Map.map (map rot60) $ taken b
  , id_of_point2D = rot60 . id_of_point2D b
  , neighbours = map rot60 . neighbours b . rot300
  , free_fields = Data.Set.map rot60 $ free_fields b
  }
  where n = size b
        rot60 = HexPoint.id_of_point n . HexPoint.rotate60 . HexPoint.point_of_id n
        rot300 = HexPoint.id_of_point n . HexPoint.rotate300 . HexPoint.point_of_id n

mirror :: Int -> HexBoard -> HexBoard
mirror k b = b
  { stone = Data.Map.mapKeys fun $ stone b
  , taken = Data.Map.map (map fun) $ taken b
  , id_of_point2D = fun . id_of_point2D b
  , neighbours = map fun . neighbours b . rev
  , free_fields = Data.Set.map fun $ free_fields b
  }
  where n = size b
        fun = HexPoint.id_of_point n . HexPoint.mirror k . HexPoint.point_of_id n
        rev = HexPoint.id_of_point n . HexPoint.rorrim k . HexPoint.point_of_id n

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
