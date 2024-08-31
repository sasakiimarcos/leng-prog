module Trie  (Trie(..), left, right, find, decode, toList) where

import Bit

data Trie a = Nil | Leaf a | (Trie a) :-: (Trie a) deriving (Show, Eq)

left::Trie a -> Trie a
left Nil = error "Empty Node"
left (Leaf _) = error "Node is a Leaf"
left (l :-: _) = l

right::Trie a -> Trie a
right Nil = error "Empty Node"
right (Leaf _) = error "Node is a Leaf"
right (_ :-: r) = r
  
find::Bits -> Trie a -> a
find _ (Leaf x) = x
find (x1 : xs) t = if x1 == T then find xs (right t) else find xs (left t)

decode::Bits -> Trie Char -> String
decode bs t = decodeRecursive bs t t

decodeRecursive:: Bits -> Trie Char -> Trie Char -> String
decodeRecursive _ Nil _ = error "Leaf not found"
decodeRecursive [] (Leaf c) root = c : ""
decodeRecursive ls (Leaf c) root = c : decodeRecursive ls root root
decodeRecursive (x:xs) t root = if x == T then (decodeRecursive xs (right t) root) else (decodeRecursive xs (left t) root)
  
toList::Trie a -> [(a, Bits)]
toList t = toListRecursive t []
  
toListRecursive:: Trie a -> Bits -> [(a, Bits)]
toListRecursive (Leaf c) bs = [(c, bs)]
toListRecursive (l :-: r) bs = toListRecursive l (bs++[F]) ++ toListRecursive r (bs++[T])     
