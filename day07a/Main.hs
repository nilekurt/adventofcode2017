{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Either (rights)
import Data.List (find, foldl')
import Data.Text (Text, pack, unpack)
import Data.Tree
import qualified Data.Attoparsec.Text as AP
import qualified Data.HashMap as M

parseEntry = do
    name <- AP.takeWhile (/= ' ')
    AP.takeWhile (/= '(')
    AP.char '('
    weight <- AP.decimal :: AP.Parser Int
    AP.char ')'
    subnames <- AP.option [] (
            AP.skipWhile (/= '>')
            *> AP.string "> "
            *> AP.takeWhile (/= ',') `AP.sepBy` AP.string ", "
            )
    return (name, weight, subnames)

constructTree :: M.Map Text (a,[Text]) -> Text -> Tree a
constructTree m x = Node weight (map (constructTree m) subNodes)
    where
    (weight, subNodes) = m M.! x

main :: IO ()
main = do
    values <- lines <$> readFile "input.txt"
    let parsed = rights $ map (AP.parseOnly parseEntry . pack) values
        nameMap = foldl' (\m1 (_,_,ns) ->
                            foldl' (\m2 x -> M.insert x 1 m2) m1 ns
                         ) M.empty parsed
        rootName = find (\(x,_,_) ->
                            not (M.member x nameMap)
                        ) parsed
        nodeMap = foldr (\(x,y,z) m ->
                            M.insert x (y,z) m
                        ) M.empty parsed
    case rootName of
        Nothing      -> print "Couldn't find root node!"
        Just (x,_,_) -> putStrLn $ unpack x

