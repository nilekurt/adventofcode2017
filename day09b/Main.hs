module Main where

import qualified Data.Attoparsec.Text as AP
import Data.Text (Text, pack, unpack)

countGarbage i = do
    garbage <- AP.takeWhile (\c -> c /= '!' && c /= '>')
    let n = length $ unpack garbage
    c <- AP.anyChar
    case c of
        '!' -> do AP.anyChar; countGarbage (i+n)
        '>' -> return (i+n)

parseEntry i = do
    done <- AP.atEnd
    if done then return i
        else do
            c <- AP.anyChar
            case c of
                '!' -> do AP.anyChar; parseEntry i
                '<' -> do n <- countGarbage i; parseEntry n
                _   -> parseEntry i

splitBy n [] = []
splitBy n xs = take n xs ++ splitBy n (drop n xs)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsed = AP.parseOnly (parseEntry 0) (pack input)
    case parsed of
        Left a -> print a
        Right b -> print b
