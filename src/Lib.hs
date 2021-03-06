module Lib
    ( listOfN,
      slice,
      replace,
      removeSameValues,
      compareString,
      rotate,
      deleteAtIndex
    ) where

import Data.List
import Debug.Trace

listOfN :: Int -> a -> [a]
listOfN n x = take n (repeat x)

slice :: Int -> Int -> [a] -> [a]
slice begin end array
    | begin < 0 || end < 0 = []
    | otherwise = take ((end + 1) - begin) (drop begin array)

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace n newVal all@(x:xs)
    | n < 0 = all
    | n == 0 = newVal:xs
    | otherwise = x: replace (n - 1) newVal xs

removeSameValues :: (Eq a) => [a] -> a -> [a]
removeSameValues all@(x:xs) a
    | x == a = removeSameValues (delete a all) a
    | otherwise = all

compareString :: String -> String -> Bool
compareString [] [] = True
compareString (x:xs) (y:ys)
    | x == y = compareString xs ys
    | otherwise = False

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate all@(x:xs) shift
    | shift == 0 = all
    | shift < 0 = rotate (xs ++ [x]) (shift + 1)
    | shift > 0 = rotate ((drop (size - 1) all) ++ (take (size - 1) all)) (shift - 1)
    where size = length all

deleteAtIndex :: Int -> [a] -> [a]
deleteAtIndex i xs
    | i < 0 = []
    | i >= size = xs
    | otherwise = slice 0 (i - 1) xs ++ slice (i + 1) size xs
    where size = length xs

