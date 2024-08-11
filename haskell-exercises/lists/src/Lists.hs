module Lists (member, union, intersection, difference,
              insert, insertionSort,
              binaryToDecimal, toDecimal, toDec, decimal,
              binaryAdd, firsts) where
  
import Data.Char(digitToInt)  

member:: Int -> [Int] -> Bool
member _ []      = False
member e (x:xs)  = e == x || member e xs


union:: [Int] -> [Int] -> [Int]
union [] ys     = ys
union (x:xs) ys 
  | member x ys = union xs ys
  | otherwise   = x : union xs ys

invert:: [a] -> [a]
invert [] = []
invert (x:xs) = invert xs ++ [x]

-- Remove Implementations, from, here on

intersection:: [Int] -> [Int] -> [Int]
intersection [] _ = []
intersection (x:xs) ys
  | member x ys = x : intersection xs ys
  | otherwise = intersection xs ys

difference:: [Int] -> [Int] -> [Int]
difference [] _ = []
difference (x:xs) ys
  | member x ys = difference xs ys
  | otherwise = x : difference xs ys

insert:: Int -> [Int] -> [Int]
insert i xs
  | xs == [] = i : []
  | i <= head xs = i : xs
  | otherwise = head xs : insert i (tail xs)

insertionSort :: [Int] -> [Int]
insertionSort xs
  | xs == [] = []
  | otherwise = insert (head xs) (insertionSort (tail xs))
  
-- Implementation using foldr
-- insertionSort xs = foldr insert [] xs

binaryToDecimal :: [Int] -> Int
binaryToDecimal [] = 0
binaryToDecimal (x:xs) = (x * 2 ^ (length xs)) + binaryToDecimal xs
    
toDecimal :: Int -> [Int] -> Int
toDecimal _ [] = 0
toDecimal a (x:xs) = (x * a ^ (length xs)) + toDecimal a xs 
    
toDec::Int -> String -> Int
toDec base s = toDecimal base (map digitToInt s)

-- Same as `toDec` But use a list comprehension

decimal::Int -> String -> Int
decimal base s = sum [(digitToInt x) * base ^ ((length s) - 1 - i) | (i, x) <- zip [0..] s]
--  where
--    sum::[Int] -> Int
--    sum [] = 0
--    sum (x:xs) = x + sum xs
-- another way of implementing this is by creating a helper function to invert the list, thus avoiding the need to calculate the length of the list

firsts::[a] -> [[a]]
firsts [] = []
firsts (x:xs) = [x] : firstsRecursive xs [x]
  where
    firstsRecursive:: [a] -> [a] -> [[a]]
    firstsRecursive [] _ = []
    firstsRecursive (y:ys) zs = (zs++[y]) : firstsRecursive ys (zs ++ [y])

-- Given two String that represents numbers in binary implement the 'binaryAdd' function
-- DO NOT USE a predefined '+' operation

binaryAdd::String -> String -> String
binaryAdd [] [] = "0"
binaryAdd n1 n2 = invert (binaryAddRecursive (invert n1) (invert n2) 0)

binaryAddRecursive:: String -> String -> Int -> String
binaryAddRecursive "" "" c = if c == 0 then "" else "1"
binaryAddRecursive "" (y:ys) c = (fst (binaryDigitAdd '0' y c)) : binaryAddRecursive [] ys (snd (binaryDigitAdd '0' y c))
binaryAddRecursive (x:xs) "" c = (fst (binaryDigitAdd x '0' c)) : binaryAddRecursive xs [] (snd (binaryDigitAdd x '0' c))
binaryAddRecursive (x:xs) (y:ys) c = (fst (binaryDigitAdd x y c)) : binaryAddRecursive xs ys (snd (binaryDigitAdd x y c))

binaryDigitAdd:: Char -> Char -> Int -> (Char, Int)
binaryDigitAdd c1 c2 i
  | c1 == '0' && c2 == '0' && i == 0 = ('0', 0)
  | c1 == '0' && c2 == '0' && i == 1 = ('1', 0)
  | c1 == '0' && c2 == '1' && i == 0 = ('1', 0)
  | c1 == '0' && c2 == '1' && i == 1 = ('0', 1)
  | c1 == '1' && c2 == '0' && i == 0 = ('1', 0)
  | c1 == '1' && c2 == '0' && i == 1 = ('0', 1)
  | c1 == '1' && c2 == '1' && i == 0 = ('0', 1)
  | c1 == '1' && c2 == '1' && i == 1 = ('1', 1)
  | otherwise = ('0', 0)