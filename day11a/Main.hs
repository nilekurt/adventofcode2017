{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<|>))
import Data.Text (pack)
import Linear
import qualified Data.Attoparsec.Text as AP

cubeDistance (V3 x y z) = (maximum . map abs) [x,y,z]

toDir x = case x of
    "n"  -> V3   0   1 (-1)
    "ne" -> V3   1   0 (-1)
    "se" -> V3   1 (-1)  0
    "s"  -> V3   0 (-1)  1
    "sw" -> V3 (-1)  0   1
    "nw" -> V3 (-1)  1   0

parseEntry xs = do
    done <- AP.atEnd
    if done then return $ reverse xs
        else do
            x <- AP.string "nw"
                <|> AP.string "ne"
                <|> AP.string "n"
                <|> AP.string "sw"
                <|> AP.string "se"
                <|> AP.string "s"
            AP.anyChar
            parseEntry (toDir x : xs)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsed = AP.parseOnly (parseEntry []) (pack input)
    case parsed of
        Left a -> putStrLn "Parsing failed!"
        Right xs -> print $ cubeDistance (sum xs)
