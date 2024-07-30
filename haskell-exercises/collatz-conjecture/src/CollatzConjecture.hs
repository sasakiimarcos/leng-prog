module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = if n <= 0 then Nothing else collatzRecursive n 0
  where
    collatzRecursive num count
      | num == 1 = Just count
      | num `mod` 2 == 0 = collatzRecursive (num `div` 2) (count + 1)
      | otherwise = collatzRecursive (3 * num + 1) (count + 1)
