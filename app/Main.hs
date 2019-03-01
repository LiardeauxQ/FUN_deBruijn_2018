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
deBruijn n alphabet
    | n <= 0 = ""
    | otherwise = [alphabet !! x | x <- cleanSequence(db n (listOfN n 0))]

main :: IO ()
main = getArgs >>= parse


parse ["--check"] = usage >> exit
parse ["--unique"] = usage >> exit
parse ["--clean"] = usage >> exit
parse [] = usage >> exit
parse fs = do
        let order = read(fs !! 0) :: Int

        putStrLn(deBruijn order "01") >> exit

usage   = do
            putStrLn("USAGE: ./deBruijn n [a] [--check|--unique|--clean]\n")
            putStrLn("\t--check\t\tcheck if a sequence is a de Bruijn sequence")
            putStrLn("\t--unique\tcheck if 2 sequences are distinct de Bruijn sequences")
            putStrLn("\t--clean\t\tlist cleaning")
            putStrLn("\tn\t\torder of the sequence")
            putStrLn("\ta\t\talphabet [def: \"01\"]")

exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 84)
