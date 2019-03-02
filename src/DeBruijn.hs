module DeBruijn
    ( deBruijn,
      isDeBruijn
    ) where

import Lib
import Data.List

cleanSequence :: [Int] -> [Int]
cleanSequence xs = reverse (removeSameValues (reverse xs) 0)

db :: Int -> [Int] -> [Int]
db n all
    | isInfixOf word all == False = db n new_all
    | isInfixOf inv_word all == False = db n new_inv_all
    | otherwise = all
    where   new_all = all ++ [1]
            new_inv_all = all ++ [0]
            size = length new_all
            word = slice (size - n) size new_all
            inv_word = slice (size - n) size new_inv_all

deBruijn :: Int -> String -> String
deBruijn n alphabet
    | n <= 0 = ""
    | otherwise = [alphabet !! x | x <- cleanSequence(db n $ listOfN n 0)]


splitDBSequence :: [a] -> Int -> Int -> [[a]]
splitDBSequence all@(x:xs) n s
    | length word == 0 = []
    | nDiff > 0 = [word ++ (listOfN nDiff x)] ++ splitDBSequence all n (s + 1)
    | otherwise = word:splitDBSequence all n (s + 1)
    where word = slice s (s + n - 1) all
          nDiff = n - length word

isDeBruijn :: String -> Int -> String -> Bool
isDeBruijn xs n alphabet
    | length splitSequence == length alphabet ^ n = True
    | otherwise = False
    where splitSequence = nub $ splitDBSequence xs n 0 
