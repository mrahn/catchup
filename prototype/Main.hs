module Main (main) where

import Catchup (Catchup)
import qualified Catchup (catchup, result)

import qualified Point2D (Point2D, read_lg_move, hexangular)
import qualified Put (put)

import qualified HexPoint (id_of_point, mirror)

------------------------------------------------------------------------------

main :: IO ()
main = print $ Catchup.result $ import_lg_game lg1657875

------------------------------------------------------------------------------

import_lg_game :: [[String]] -> Catchup
import_lg_game = foldl (flip Put.put) (Catchup.catchup 5) . points_from_lg

points_from_lg :: [[String]] -> [[Point2D.Point2D]]
points_from_lg = map (map (Point2D.read_lg_move 5))

c :: [[String]] -> String
c = tail . concat
  . map ( \ xs -> ",{" ++ (tail $ init $ show xs) ++ "}")
  . map (map (HexPoint.id_of_point 5 . HexPoint.mirror 2 . Point2D.hexangular 5))
  . points_from_lg


lg1657875 :: [[String]]
lg1657875 = [ ["E5"]
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

lg1657870 :: [[String]]
lg1657870 = [ ["D3"]
            , ["H3","C6"]
            , ["E4","F5"]
            , ["F4","E6"]
            , ["C8","G6"]
            , ["F7","D4"]
            , ["G3","F2"]
            , ["G2","G5"]
            , ["C5","B7"]
            , ["G4","E5"]
            , ["B6","C4","F3"]
            , ["C7","B8"]
            , ["H2","I2"]
            , ["E3","D5"]
            , ["E2","I3","I4"]
            , ["H5","I5","A8"]
            , ["A7","F6","E7"]
            , ["D8", "E8"]
            ]

lg1657867 :: [[String]]
lg1657867 = [ ["C5"]
            , ["F5","H5"]
            , ["G5","F7"]
            , ["H3","F3"]
            , ["F4","E6"]
            , ["B7","D3"]
            , ["E3","C7"]
            , ["D7","D5"]
            , ["E4","G4"]
            , ["D6","D4","G3"]
            , ["C6","D8"]
            , ["H4","F6"]
            , ["E2","G6"]
            , ["D2","E1","F1"]
            , ["C8","C4"]
            , ["H6","G7"]
            , ["C3","E9"]
            , ["F8","E8"]
            , ["G1","F2","E7"]
            , ["D9","C9","B9"]
            , ["B8","A9"]
            , ["G2","H1","B5"]
            , ["B6","A6"]
            ];
