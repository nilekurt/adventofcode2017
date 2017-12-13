module Main where

import qualified Data.Array.IO as A

advance :: A.IOArray Int Int -> Int -> Integer -> IO Integer
advance a ip n = do
    (min,max) <- A.getBounds a
    if ip < min || ip > max then return n else
        do
        nip <- A.readArray a ip
        if nip > 2 then
            A.writeArray a ip (nip-1)
        else
            A.writeArray a ip (nip+1)
        advance a (ip + nip) (n+1)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let values = map read $ lines input :: [Int]
    instructions <- A.newListArray (0, length values - 1) values
    answer <- advance instructions 0 0
    print answer
