module Main where

import Data.List (nub)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let answer = length $ filter (\ws -> length (nub ws) == length ws) $ (map words . lines) input 
    print answer
