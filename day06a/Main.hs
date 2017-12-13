module Main where

import Data.List.Zipper
import Data.List
import Control.Monad (foldM)
import Control.Monad.ST
import Data.Array.ST
import Data.Maybe (fromMaybe)

input = [14,0,15,12,11,11,3,5,1,6,8,4,9,1,8,4] :: [Int]

redist :: (Int, STArray s Int Int) -> Int -> ST s (Int,STArray s Int Int)
redist (i,a) _ = do
    (min,max) <- getBounds a
    let j = rem i (max - min + 1)
    x <- readArray a j
    writeArray a j (x+1)
    return (j+1,a)

reallocate :: [Int] -> [Int]
reallocate xs = runST $ do
    let n = length xs
    a <- newListArray (0, n-1) xs :: ST s (STArray s Int Int)
    let i = fromMaybe (-1) $ elemIndex (maximum xs) xs
    x <- readArray a i
    writeArray a i 0
    (_,result) <- foldM redist (i+1,a) [x,x-1..1]
    getElems result

findDuplicate :: Zipper [Int] -> Int -> Int
findDuplicate (Zip xs (y:ys)) n
    | y `elem` xs = n
    | otherwise   = findDuplicate (Zip (y:xs) ys) (n+1)

main :: IO ()
main = do
    let values = iterate reallocate input
        lz = fromList values
    print $  findDuplicate lz 0
