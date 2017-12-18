module Main where

import Data.List (nub,permutations)

checkPermuts x y = x `elem` permutations y

foldfunc x y s = not (x /= y && checkPermuts x y) && s

main :: IO ()
main = do
    input <- readFile "input.txt"
    let phrases = (map words . lines) input :: [[String]]
        deduped = filter (\p -> length (nub p) == length p) phrases
        answer = length $ filter (and . (\p -> map (\x -> foldr (foldfunc x) True p) p)) deduped
    print answer
