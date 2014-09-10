module Point2D
  (Point2D, rectangluar, hexangular, coordinate_x, read_lg_move)
where

import qualified HexPoint (HexPoint (HexPoint))

import qualified Data.Char (ord, chr)

------------------------------------------------------------------------------

data Point2D = Point2D Int Int

instance Show Point2D where
  showsPrec _ (Point2D x y) = (:) (Data.Char.chr (x + 97)) . (++) (show y)

instance Read Point2D where
  readsPrec _ (x:xs) = [(Point2D (Data.Char.ord x - 97) (read xs), [])]

rectangluar :: Int -> HexPoint.HexPoint -> Point2D
rectangluar n (HexPoint.HexPoint x y _) = Point2D (pred n + x) (pred n + y + min 0 x)

hexangular :: Int -> Point2D -> HexPoint.HexPoint
hexangular n (Point2D x y) = HexPoint.HexPoint hx hy hz
  where hx = x - pred n
        hy = y - pred n - min 0 hx
        hz = -(hx + hy)

coordinate_x :: Point2D -> Int
coordinate_x (Point2D x _) = x

------------------------------------------------------------------------------

read_lg_move :: Int -> String -> Point2D
read_lg_move n (c:xs) = Point2D x (shift y)
  where x = (Data.Char.ord c - 65)
        y = read xs - 1
        shift = (+) (min 0 (x - pred n))
