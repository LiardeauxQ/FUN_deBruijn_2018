module Main where

import Lib
import DeBruijn
import Data.List
import Data.Maybe
import Text.Read
import Debug.Trace
import System.Environment
import System.Exit

checkSequenceConformity :: String -> String -> Bool
checkSequenceConformity [] [] = False
checkSequenceConformity [] _  = True
checkSequenceConformity (x:xs) alphabet
    | isInfixOf (x:[]) alphabet = checkSequenceConformity xs alphabet
    | otherwise = False

execCheckOption :: String -> String -> Maybe Int -> IO ()
execCheckOption sequence alphabet order = do
        if checkSequenceConformity sequence alphabet == False
            then usage
        else
            case order of
                Just n -> if isDeBruijn sequence n alphabet
                            then putStrLn("OK")
                        else putStrLn("KO")
                Nothing -> usage


main :: IO ()
main = getArgs >>= parse 

parse [n, alphabet, "--check"]  = do
        line <- getLine
        execCheckOption line alphabet (readMaybe(n) :: Maybe Int)
        
parse [n, alphabet, "--unique"] = usage
parse [n, alphabet, "--clean"]  = usage
parse [n, "--check"]            = do
        line <- getLine
        execCheckOption line "01" (readMaybe(n) :: Maybe Int)

parse [n, "--unique"]           = usage
parse [n, "--clean"]            = usage


parse [n, alphabet]             = do
        let order = readMaybe(n) :: Maybe Int

        case order of
            Just x -> if length alphabet <= 1
                        then usage >> quitFailure
                      else putStrLn(deBruijn x alphabet)
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

