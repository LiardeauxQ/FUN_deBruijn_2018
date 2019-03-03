module DeBruijn
    ( deBruijn,
      isDeBruijn,
      isSameDeBruijn
    ) where

import Lib
import Data.List
import Debug.Trace

cleanSequence :: [Int] -> [Int]
cleanSequence xs = reverse (removeSameValues (reverse xs) 0)

db :: Int -> Int -> Int -> [Int] -> [Int]
db alphabet_size n index all
    | index < 0 = all
    | isInfixOf word all == False = db alphabet_size n alphabet_size new_all
    | otherwise = db alphabet_size n (index - 1) all
    where new_all = all ++ [index]
          size = length new_all
          word = slice (size - n) size new_all


deBruijn :: Int -> String -> String
deBruijn n alphabet
    | n <= 0 = ""
    | otherwise = [alphabet !! x | x <- cleanSequence(db size n size $ listOfN n 0)]
    where size = length alphabet - 1


splitDBSequence :: [a] -> Int -> Int -> [[a]]
splitDBSequence all@(x:xs) n s
    | length word == 0 = []
    | nDiff > 0 = [word ++ (listOfN nDiff x)] ++ splitDBSequence all n (s + 1)
    | otherwise = word:splitDBSequence all n (s + 1)
    where word = slice s (s + n - 1) all
          nDiff = n - length word


isDeBruijn :: String -> Int -> String -> Bool
isDeBruijn xs n alphabet
    | length xs /= length alphabet ^ n = False
    | length splitSequence == length alphabet ^ n = True
    | otherwise = False
    where splitSequence = nub $ splitDBSequence xs n 0 


isSameDeBruijn :: String -> String -> Int -> Bool
isSameDeBruijn [] [] _ = False
isSameDeBruijn seq1@(x:xs) seq2@(y:ys) shift
    | shift == length seq2 = True
    | compareString seq1 seq2 = False
    | otherwise = isSameDeBruijn seq1 (rotate seq2 1) (shift + 1)
