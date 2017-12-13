module Main where

despiral :: Integral a => a -> a -> (a,a)
despiral _ 1 = (0,0)
despiral i n
    | y <= 0 = (j, abs $ rem pos (i-1))
    | otherwise = despiral (i+2) n
    where
    y = n - i^2
    j = quot i 2
    pos = n - (i-2)^2 - j

manhattanDistance (x,y) = x + y

main :: IO ()
main = print $ manhattanDistance $ despiral 1 265149

