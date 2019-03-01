module Lib
    ( listOfN,
      slice,
      replace,
      removeSameValues
    ) where

import Data.List

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
