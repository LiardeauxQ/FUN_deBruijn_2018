module Main where

import Lib
import Data.List
--import Debug.Trace
import System.Environment
import System.Exit


cleanSequence :: [Int] -> [Int]
cleanSequence xs = reverse (removeSameValues (reverse xs) 0)

db :: Int -> [Int] -> [Int]
db n all
    | isInfixOf word all == False = db n new_all
    | isInfixOf inv_word all == False = db n new_inv_all
    | otherwise = all
    where   size = length all
            new_all = all ++ [1]
            new_inv_all = all ++ [0]
            word = slice (size + 1 - n) (size + 1) new_all
            inv_word = slice (size + 1 - n) (size + 1) new_inv_all

deBruijn :: Int -> String -> String
deBruijn n alphabet = [alphabet !! x | x <- cleanSequence(db n (listOfN n 0))]

main :: IO ()
main = putStrLn("test")
