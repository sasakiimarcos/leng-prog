module Frequencies  (Frequency, frequencyMap, frequencies, insert, insertionSort) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Tuple(swap)

type Frequency = (Int, Char)

frequencies::String -> [Frequency]
frequencies s =  insertionSort [ (i, c) | (c, i) <- (Map.toList $ frequencyMap s) ]

frequencyMap::(Ord a) => [a] -> Map a Int
frequencyMap ls = _frequencyMap ls Map.empty

_frequencyMap::(Ord a) => [a] -> Map a Int -> Map a Int
_frequencyMap [] m = m
_frequencyMap (x:xs) m = _frequencyMap xs (Map.insertWith (+) x 1 m)

insert::(Ord a) => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs)
  | a <= x = a : x : xs
  | otherwise = x : (insert a xs)

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort ls = foldr insert [] ls