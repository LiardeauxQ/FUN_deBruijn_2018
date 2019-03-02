module Main where

import Lib
import Data.List
import Data.Maybe
import Text.Read
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

parse [n, alphabet, "--check"]  = usage >> exit
parse [n, alphabet, "--unique"] = usage >> exit
parse [n, alphabet, "--clean"]  = usage >> exit
parse [n, "--check"]            = usage >> exit
parse [n, "--unique"]           = usage >> exit
parse [n, "--clean"]            = usage >> exit
parse [n, alphabet]             = do
        let order = readMaybe(n) :: Maybe Int

        case order of
            Just x -> if length alphabet <= 1
                        then usage >> quitFailure
                      else putStrLn(deBruijn x alphabet) >> exit
            Nothing -> usage >> quitFailure
parse [n]                       = do
        let order = readMaybe(n) :: Maybe Int

        case order of
            Just n -> putStrLn(deBruijn n "01") >> exit
            Nothing -> usage >> quitFailure
parse []                        = usage >> quitFailure
parse otherwise                 = usage >> quitFailure

usage   = do
            putStrLn("USAGE: ./deBruijn n [a] [--check|--unique|--clean]\n")
            putStrLn("\t--check\t\tcheck if a sequence is a de Bruijn sequence")
            putStrLn("\t--unique\tcheck if 2 sequences are distinct de Bruijn sequences")
            putStrLn("\t--clean\t\tlist cleaning")
            putStrLn("\tn\t\torder of the sequence")
            putStrLn("\ta\t\talphabet [def: \"01\"]")

exit            = exitWith ExitSuccess
quitFailure     = exitWith (ExitFailure 84)
