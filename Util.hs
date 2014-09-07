module Util (join, select, unique) where

import qualified Data.List (intersperse)
import qualified Data.Set (empty, member, insert)

------------------------------------------------------------------------------

join :: [a] -> [[a]] -> [a]
join sep = concat . Data.List.intersperse sep

select :: Int -> [a] -> [[a]]
select 0 _ = [[]]
select _ [] = []
select n (x:xs) = [ (x:ys) | ys <- select (pred n) xs ] ++ select n xs

unique :: Ord a => [a] -> [a]
unique = unique_ Data.Set.empty
  where unique_ _ [] = []
        unique_ cache (x:xs)
          | Data.Set.member x cache = unique_ cache xs
          | otherwise = x : unique_ (Data.Set.insert x cache) xs
