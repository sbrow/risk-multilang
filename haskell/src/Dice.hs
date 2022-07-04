module Dice (d, dice, total) where

infixl 5 `d`
d :: Integer -> Integer -> [[Integer]]
n `d` s = dice n s

-- Returns the entire possibility space of rolling d dice with sides sides.
dice :: Integer -> Integer -> [[Integer]]
dice d sides
    | d == 1 = [ [d] | d<-[1..sides]]
    | otherwise = combine (dice (d-1) sides) (dice 1 sides)

-- Returns the total possible dice combinations
total :: Integer -> Integer -> Integer
total d sides
    | d == 1 = sides
    | otherwise = (total (d-1) sides) * sides

-- Get the union of all combinations of set a with set b
combine :: [[Integer]] -> [[Integer]] -> [[Integer]]
combine a b = [ x ++ y | x<-a, y<-b ]