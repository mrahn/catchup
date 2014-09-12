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

breakL :: (a -> Bool) -> [a] -> [[a]]
breakL sep xs = case break sep xs of (l,[]) -> [l]
                                     (l,_:rs) -> l : breakL sep rs

splitAtL :: Int -> [a] -> [[a]]
splitAtL m xs = case splitAt m xs of (l,[]) -> [l]
                                     (l,rs) -> l : splitAtL m rs

lg_from_string :: String -> [[String]]
lg_from_string = map (splitAtL 2 . tail . dropWhile (/='.')) . breakL (==' ')

lg1657873 :: String
lg1657873 = "1.G4 2.F5E7 3.E5D7 4.D6E4 5.F3D4 6.C5G3 7.D5E6 8.F6F4E3 9.D8E8F2 10.I2D2 11.B6G7 12.C6H5 13.B7E2 14.D3C4 15.G5G6F7 16.C8I3 17.H3H2 18.G2H1C7 19.B8B9C9 20.I1B5A6 21.A7H4I4 22.E1F1 23.G1D9I5 24.B4C3 25.H6F8E9 26.A5A8A9"

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

lg1657870 :: String
lg1657870 = "1.D3 2.H3C6 3.E4F5 4.F4E6 5.C8G6 6.F7D4 7.G3F2 8.G2G5 9.C5B7 10.G4E5 11.B6C4F3 12.C7B8 13.H2I2 14.E3D5 15.E2I3I4 16.H5I5A8 17.A7F6E7 18.D8E8 19.D7H6 20.C9B9"

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
