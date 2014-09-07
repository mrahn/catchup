module Player (Player (Blue, Orange), other) where

------------------------------------------------------------------------------

data Player = Blue | Orange deriving (Eq, Ord)

other :: Player -> Player
other Blue = Orange
other Orange = Blue

instance Show Player where
  showsPrec _ Blue = (++) "B"
  showsPrec _ Orange = (++) "O"
