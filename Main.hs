module Main (main) where

import Catchup (Catchup)
import qualified Catchup (catchup, result)

import qualified Point2D (read_lg_move)
import qualified Put (put)

------------------------------------------------------------------------------

main :: IO ()
main = print $ Catchup.result $ lg1657875

------------------------------------------------------------------------------

import_lg_game :: [[String]] -> Catchup
import_lg_game = foldl (flip Put.put) (Catchup.catchup 5) . map (map (Point2D.read_lg_move 5))

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
