module Main where

import qualified Data.Attoparsec.Text as AP
import Data.Text (Text, pack, unpack)

skipGarbage = do
    AP.skipWhile (\c -> c /= '!' && c /= '>')
    c <- AP.anyChar
    case c of
        '!' -> do AP.anyChar; skipGarbage
        '>' -> return ()

parseEntry i xs = do
    done <- AP.atEnd
    if done then return xs
        else do
            c <- AP.anyChar
            case c of
                '!' -> do AP.anyChar; parseEntry i xs
                '<' -> do skipGarbage; parseEntry i xs
                '{' -> parseEntry (i+1) xs
                '}' -> parseEntry (i-1) (i:xs)
                _   -> parseEntry i xs

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsed = AP.parseOnly (parseEntry 0 []) (pack input)
    case parsed of
        Left a -> print a
        Right b -> print $ sum b
