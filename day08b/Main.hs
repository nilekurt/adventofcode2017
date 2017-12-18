{-# LANGUAGE RankNTypes, OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad (when)
import Data.Either (rights)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Attoparsec.Text as AP
import qualified Data.HashTable.IO as H

type Register = String
type Comparison = (Int -> Bool)
type Operation = (Int -> Int)
data Condition = Cond Register Comparison
data Action = Act Register Operation

data Expression = Exp Action Condition
instance Show Expression where
    show (Exp (Act x y) (Cond s t)) = show x ++ " op " ++ show s ++ " cond"


parseComparison :: Text -> Int -> Comparison
parseComparison name val = case name of
                    "!=" -> (/= val)
                    "==" -> (== val)
                    "<"  -> (<  val)
                    "<=" -> (<= val)
                    ">"  -> (>  val)
                    ">=" -> (>= val)

parseOperation :: Text -> Int -> Operation
parseOperation name val = case name of
                    "inc" -> (+ val)
                    "dec" -> \x -> x - val

parseEntry = do
    opReg <- AP.takeWhile (/= ' ')
    AP.char ' '
    op <- AP.takeWhile (/= ' ')
    AP.char ' '
    opVal <- AP.signed AP.decimal
    AP.string " if "
    cmpReg <- AP.takeWhile (/= ' ')
    AP.char ' '
    cmp <- AP.takeWhile (/= ' ')
    AP.char ' '
    cmpVal <- AP.signed AP.decimal
    AP.takeText
    return $ Exp (Act  (unpack opReg)  (parseOperation op opVal))
                 (Cond (unpack cmpReg) (parseComparison cmp cmpVal))

type HashTable k v = H.CuckooHashTable k v
eval :: HashTable String Int -> Expression -> IO ()
eval ht (Exp (Act opReg op) (Cond condReg cond))  =
    do
    x <- H.lookup ht condReg
    when (cond $ fromMaybe 0 x) $
        do
        y <- H.lookup ht opReg
        let final = op $ fromMaybe 0 y
        H.insert ht opReg final
        max <- H.lookup ht "maximum"
        when (final > fromMaybe 0 max) $ H.insert ht "maximum" final

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    let expList = rights $ map (AP.parseOnly parseEntry . pack) input
    ht <- H.new
    mapM_ (eval ht) expList
    max <- H.lookup ht "maximum"
    print $ fromMaybe 0 max
