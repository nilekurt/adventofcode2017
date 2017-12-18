{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Attoparsec.Text as AP
import qualified Data.HashTable.IO as H

pairs = [("ne","sw"),("nw","se"),("n","s")]
halfPairs = [("nw","s","sw"), ("ne","s","se"), ("sw","n","nw"), ("se","n","ne")]

parseEntry xs = do
    done <- AP.atEnd
    if done then return $ reverse xs
        else do
            x <- AP.string "nw"
                <|> AP.string "ne"
                <|> AP.string "sw"
                <|> AP.string "se"
                <|> AP.string "n"
                <|> AP.string "s"
            AP.anyChar
            parseEntry (x:xs)


maybeAdd x y = case y of
        Nothing -> (Just 1, ())
        Just n  -> (Just (n+x), ())

type HashTable k v = H.CuckooHashTable k v

reduce ht x y = do
    mx <- H.lookup ht x
    my <- H.lookup ht y
    let nx = fromMaybe 0 mx
        ny = fromMaybe 0 my
        small = min nx ny
    H.insert ht x (nx - small)
    H.insert ht y (ny - small)
    return small

reducePairs ht = mapM_ (uncurry $ reduce ht) pairs

reduceHalfPairs ht = mapM_ (\(x,y,z) -> do
                    small <- reduce ht x y
                    H.mutate ht z (maybeAdd small)) halfPairs

countDirections ht = mapM_ (\x -> H.mutate ht x (maybeAdd 1))

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsed = AP.parseOnly (parseEntry []) (pack input)
    case parsed of
        Left a -> print a
        Right b -> do
            ht <- H.new :: IO (HashTable Text Int)
            countDirections ht b
            reducePairs ht
            reduceHalfPairs ht
            xs <- H.toList ht
            mapM_ print xs
            print $ sum $ map snd xs
