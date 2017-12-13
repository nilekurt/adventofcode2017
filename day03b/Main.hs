module Main where

import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.HashMap as M

spiralCoords :: [(Int, Int)]
spiralCoords = (0,0) : spiralGen 1

spiralGen :: Int -> [(Int,Int)]
spiralGen n = first ++ second ++ third ++ fourth ++ spiralGen (n+1)
    where
    first = map (\y -> (max, y)) [min+1..max]
    second = map (\x -> (x, max)) [max-1, max-2..min]
    third = map (\y -> (min, y)) [max-1, max-2..min]
    fourth = map (\x -> (x,min)) [min+1..max]
    max = n
    min = -n

getAdjacents (x,y) = [(x-1,y-1),(x-1, y),(x-1, y+1)
                     ,(x, y-1),(x,y+1),
                     (x+1,y-1),(x+1,y),(x+1,y+1)]

calculateSpiral :: [Int]
calculateSpiral = calcNext initial 1
    where
    initial = M.fromList [((0,0),1)]

calcNext :: M.Map (Int,Int) Int -> Int -> [Int]
calcNext m i = next : calcNext (M.insert r next m) (i+1)
    where
    next = foldr (\k s-> s + fromMaybe 0 (M.lookup k m)) 0 (getAdjacents r)
    r = spiralCoords !! i

input = 265149

main :: IO ()
main = print $ fromMaybe 0 $ find (>input) calculateSpiral

