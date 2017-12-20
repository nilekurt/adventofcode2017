{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<|>))
import Control.Monad.Extra (concatMapM)
import Data.List (nub, sort, union)
import Data.Text (pack)
import qualified Data.Array.IO as A
import qualified Data.Attoparsec.Text as AP

parseEntry = do
    AP.decimal
    AP.string " <-> "
    AP.option [] $ AP.decimal `AP.sepBy` AP.string ", "

buildGroup :: A.IOArray Int [Int] -> [Int] -> [Int] -> IO [Int]
buildGroup _   xs [] = return $ sort xs
buildGroup arr xs ys = do
    next <- concatMapM (A.readArray arr) ys
    let new = filter (`notElem` xs) next
    buildGroup arr (xs `union` next) new

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    let parsed = mapM (AP.parseOnly parseEntry . pack) input
    case parsed of
        Left _ -> putStrLn "Parsing failed!"
        Right xs -> do
            let ub = length xs - 1
            arr <- A.newListArray (0, ub) xs :: IO (A.IOArray Int [Int])
            result <- mapM (buildGroup arr [] . (:[])) [0..ub]
            print $ length $ nub result
