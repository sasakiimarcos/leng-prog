module Fraction (Fraction, add, sub, mul, divide, hcf) where

type Fraction = (Int, Int)

-- Implement the `add` Function

numerator :: Fraction -> Int
numerator (n, _) = n

denominator :: Fraction -> Int
denominator (_, d) = d

add :: Fraction -> Fraction -> Fraction
add n d = (num, den)
  where
    n1 = numerator n
    n2 = numerator d
    d1 = denominator n
    d2 = denominator d
    num = n1 * d2 + n2 * d1
    den = d1 * d2

-- Implement the `sub` Function

sub :: Fraction -> Fraction -> Fraction
sub n d = (num, den)
  where
    n1 = numerator n
    n2 = numerator d
    d1 = denominator n
    d2 = denominator d
    num = n1 * d2 - n2 * d1
    den = d1 * d2

-- Implement the `mul` Function

mul :: Fraction -> Fraction -> Fraction
mul n d = (num, den)
  where
    n1 = numerator n
    n2 = numerator d
    d1 = denominator n
    d2 = denominator d
    num = n1 * n2
    den = d1 * d2

-- Implement the `divide` Function

divide :: Fraction -> Fraction -> Fraction
divide n d = (num, den)
  where
    n1 = numerator n
    n2 = numerator d
    d1 = denominator n
    d2 = denominator d
    num = n1 * d2
    den = n2 * d1

-- Implement the `hcf` Function

hcf :: Int -> Int -> Int
hcf n d
  | n == 0 = d
  | d == 0 = n
  | n > d = hcf (n - d) d
  | otherwise = hcf n (d - n) 

    