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
checkSequenceConformity [] _ = True
checkSequenceConformity (x:xs) alphabet
    | length alphabet <= 1 = False
    | isInfixOf (x:[]) alphabet = checkSequenceConformity xs alphabet
    | otherwise = False

-- Check Option --

execCheckOption :: String -> String -> Maybe Int -> IO ()
execCheckOption sequence alphabet order = do
        if length alphabet <= 1
            then usage >> quitFailure
        else if checkSequenceConformity sequence alphabet == False
            then putStrLn("KO")
        else
            case order of
                Just n -> if isDeBruijn sequence n alphabet
                            then putStrLn("OK")
                        else putStrLn("KO")
                Nothing -> usage >> quitFailure

-- Unique Option --

execUniqueOption :: String -> String -> String -> Maybe Int -> IO ()
execUniqueOption seq1 seq2 alphabet order = do
        if checkSequenceConformity seq1 alphabet == False
            && checkSequenceConformity seq2 alphabet == False
            then usage >> quitFailure
        else
            case order of
                Just n -> if isDeBruijn seq1 n alphabet
                          && isDeBruijn seq2 n alphabet
                          && isSameDeBruijn seq1 seq2 0
                            then putStrLn("OK")
                        else putStrLn("KO")
                Nothing -> usage >> quitFailure

-- Clean Option --

getSequenceArray :: String -> Int -> IO [String]
getSequenceArray alphabet order = do
        line <- getLine
        if line == "END"
            then return []
        else do
            nextArray <- getSequenceArray alphabet order
            if isDeBruijn line order alphabet == False
                then return (nextArray)
            else
                return (line:nextArray)


compareUniqueSequences :: String -> [String] -> Bool
compareUniqueSequences _ [] = True
compareUniqueSequences sequence (x:xs)
    | isSameDeBruijn sequence x 0 == False = False
    | otherwise = compareUniqueSequences sequence xs


parseUniqueSequences :: [String] -> Int -> [String]
parseUniqueSequences [] _ = []
parseUniqueSequences xs index
    | index >= length xs = []
    | compareUniqueSequences (xs !! index) cleanArray = (xs !! index):parsingArray
    | otherwise = parsingArray
    where cleanArray = take index xs
          parsingArray = parseUniqueSequences xs (index + 1)


checkUSecsConform :: [String] -> String -> Int -> [String]
checkUSecsConform [] _ _ = []
checkUSecsConform (x:xs) alphabet order 
    | length alphabet ^ order == size = x:checkUSecsConform xs alphabet order
    | otherwise = checkUSecsConform xs alphabet order
    where size = length x


execCleanOption :: String -> Maybe Int -> IO ()
execCleanOption alphabet order = do 
        case order of
            Just n -> do
                sequences <- getSequenceArray alphabet n
                mapM_ putStrLn $ checkUSecsConform (parseUniqueSequences sequences 0) alphabet n
            Nothing -> usage >> quitFailure

-- Main --

main :: IO ()
main = getArgs >>= parse 

-- Parse Arguments --

parse [n, alphabet, "--check"]  = do
        line <- getLine
        execCheckOption line alphabet (readMaybe(n) :: Maybe Int)

parse [n, alphabet, "--unique"] = do
        seq1 <- getLine
        seq2 <- getLine
        execUniqueOption seq1 seq2 alphabet (readMaybe(n) :: Maybe Int)

parse [n, alphabet, "--clean"]  = execCleanOption alphabet (readMaybe(n) :: Maybe Int)
parse [n, "--check"]            = do
        line <- getLine
        execCheckOption line "01" (readMaybe(n) :: Maybe Int)

parse [n, "--unique"]           = do
        seq1 <- getLine
        seq2 <- getLine
        execUniqueOption seq1 seq2 "01" (readMaybe(n) :: Maybe Int)

parse [n, "--clean"]            = execCleanOption "01" (readMaybe(n) :: Maybe Int)

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

-- Usage --

usage   = do
            putStrLn("USAGE: ./deBruijn n [a] [--check|--unique|--clean]\n")
            putStrLn("\t--check\t\tcheck if a sequence is a de Bruijn sequence")
            putStrLn("\t--unique\tcheck if 2 sequences are distinct de Bruijn sequences")
            putStrLn("\t--clean\t\tlist cleaning")
            putStrLn("\tn\t\torder of the sequence")
            putStrLn("\ta\t\talphabet [def: \"01\"]")


exit            = exitWith ExitSuccess
quitFailure     = exitWith (ExitFailure 84)
