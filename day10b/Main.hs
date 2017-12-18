module Main where

import Data.Bits (xor)
import Data.Char (ord,chr)
import Data.List
import Numeric (showHex)

input = "31,2,85,1,80,109,35,63,98,255,0,13,105,254,128,33"

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

splitBy n [] = []
splitBy n xs = take n xs : splitBy n (drop n xs)

hashString rounds xs = map (foldl1' xor) $ splitBy 16 sparse
        where
        (_,_,sparse) = foldl' hash (0, 0, [0..255]) $ take (rounds * length realInput) cycled
        padding = [17,31,73,47,23]
        realInput = map ord xs ++ padding
        cycled = cycle realInput

digitify x
    | x < 10 = chr (x + 48)
    | otherwise = case x of
        10 -> 'a'
        11 -> 'b'
        12 -> 'c'
        13 -> 'd'
        14 -> 'e'
        15 -> 'f'

hexByte x = map digitify [quot x 16, rem x 16]

main :: IO ()
main = do
    let result = hashString 64 input
    putStrLn $ concatMap hexByte result
