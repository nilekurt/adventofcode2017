module Main where

import Data.List

input = [31,2,85,1,80,109,35,63,98,255,0,13,105,254,128,33] :: [Int]

hash (pos, skip, xs) len = (newPos, skip+1, result)
    where
    result = if excess < 0 then
                take pos xs ++ reverse (take len (drop pos xs)) ++ drop (pos + len) xs
             else
                drop (length twist - excess) twist
                    ++ take (pos - excess) (drop excess xs)
                    ++ take (length twist - excess) twist
    twist = reverse (drop pos xs ++ take excess xs)
    excess = pos + len - length xs
    newPos = rem (pos + len + skip) (length xs)

main :: IO ()
main = do
    let (_,_,x:y:_) = foldl' hash (0,0,[0..255]) input
    print $ x*y
