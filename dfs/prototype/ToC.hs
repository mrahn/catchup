module Main (main) where

import qualified Point2D (Point2D, read_lg_move, hexangular)

import qualified HexPoint (id_of_point, mirror)

import System.Environment (getArgs)

------------------------------------------------------------------------------

main :: IO ()
main = do
  putStr "lg ({"
  getArgs >>= mapM_ putStr . map (c . lg_from_string)
  putStrLn "});"

------------------------------------------------------------------------------

points_from_lg :: [[String]] -> [[Point2D.Point2D]]
points_from_lg = map (map (Point2D.read_lg_move 5))

c :: [[String]] -> String
c = tail . concat
  . map ( \ xs -> ",{" ++ (tail $ init $ show xs) ++ "}")
  . map (map (HexPoint.id_of_point 5 . HexPoint.mirror 2 . Point2D.hexangular 5))
  . points_from_lg

breakL :: (a -> Bool) -> [a] -> [[a]]
breakL sep xs = case break sep xs of (l,[]) -> [l]
                                     (l,_:rs) -> l : breakL sep rs

splitAtL :: Int -> [a] -> [[a]]
splitAtL m xs = case splitAt m xs of (l,[]) -> [l]
                                     (l,rs) -> l : splitAtL m rs

lg_from_string :: String -> [[String]]
lg_from_string = map (splitAtL 2 . tail . dropWhile (/='.')) . breakL (==' ')
