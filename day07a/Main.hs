module Main where

import qualified Data.Attoparsec.Text as AP
import qualified Data.HashMap as M
import Control.Applicative
import Data.List (find, foldl')
import Data.Either (rights)
import Data.Text (pack, Text)
import Data.Tree

parseEntry = do
    name <- AP.takeWhile (/= ' ')
    AP.takeWhile (/= '(')
    AP.char '('
    weight <- AP.decimal :: AP.Parser Int
    AP.char ')'
    subnames <- AP.option [] (
            AP.skipWhile (/= '>')
            *> AP.char '>'
            *> AP.char ' '
            *> AP.takeWhile (/= ',') `AP.sepBy` AP.string (pack ", ")
            )
    return (name,weight,subnames)

constructTree :: M.Map Text (a,[Text]) -> Text -> Tree (Text,a)
constructTree m x = Node (x,weight) (map (constructTree m) subNodes)
    where
    (weight,subNodes) = m M.! x

main :: IO ()
main = do
    values <- lines <$> readFile "input.txt"
    let parsed = rights $ map (AP.parseOnly parseEntry . pack) values
        nameMap = foldl' (\m1 (_,_,ns)->
                    foldl' (\m2 x-> M.insert x 1 m2) m1 ns
                    ) M.empty parsed
        rootName = find (\(x,_,_)-> not $ M.member x nameMap) parsed
        nodeMap = foldl' (\m (x,y,z)-> M.insert x (y,z) m) M.empty parsed
    case rootName of
        Just (x,_,_) -> do
            print $ constructTree nodeMap x
            print x
        Nothing      ->
            print "Couldn't find root node!"
