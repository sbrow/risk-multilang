#!/usr/bin/env stack
-- stack runghc

import Risk

maximum' :: Ord a => [a] -> a
maximum' = foldr1 (\x y ->if x >= y then x else y)

maximum'' :: Ord a => [a] -> a
maximum'' [x]       = x
maximum'' (x:x':xs) = maximum' ((if x >= x' then x else x'):xs)

a :: [Int]
a = [3,5,8,6]

main :: IO ()
main = print $ maximum'' a
-- main = print $ possibleOutcomes 3 3 
-- main = print $ Result(3, 5) + Result(1, 2)
