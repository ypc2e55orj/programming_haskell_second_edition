{-# OPTIONS -Wall #-}

module Main where

-- >>> 2 + 3 * 4
-- 14

-- >>> (2 + 3) * 4
-- 20

-- >>> sqrt (3^2 + 4^2)
-- 5.0

-- >>> head [1, 2, 3, 4, 5]
-- 1

-- >>> tail [1, 2, 3, 4, 5]
-- [2,3,4,5]

-- >>> [1, 2, 3, 4, 5] !! 2
-- 3

-- >>> drop 3 [1, 2, 3, 4, 5]
-- [4,5]

-- >>> length [1, 2, 3, 4, 5]
-- 5

-- >>> sum [1, 2, 3, 4, 5]
-- 15

-- >>> product [1, 2, 3, 4, 5]
-- 120

-- >>> [1, 2, 3] ++ [4, 5]
-- [1,2,3,4,5]

-- >>> reverse [1, 2, 3, 4, 5]
-- [5,4,3,2,1]

double :: Num a => a -> a
double x = x + x

-- >>> quadruple 10
-- 40
quadruple :: Num a => a -> a
quadruple x = double (double x)

-- >>> take (double 2) [1, 2, 3, 4, 5]
-- [1,2,3,4]

-- >>> factorial 10
-- 3628800
factorial :: (Num a, Enum a) => a -> a
factorial n = product [1 .. n]

-- >>> average [1, 2, 3, 4, 5]
-- 3
average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns

{- 1.2.7 -}
-- 2.
-- (2^3) * 4
-- (2 * 3) + (4 * 5)
-- 2 + (3 * (4^5))

-- 3.
-- >>> hoge
-- 2
hoge :: Int
hoge = a `div` length xs
  where a = 10
        xs = [1, 2, 3, 4, 5] :: [Int]

-- 4.
-- >>> last' [1, 2, 3, 4, 5]
-- 5
last' :: [p] -> p
last' [x] = x
last' (_:xs) = last' xs

-- >>> last'' [1, 2, 3, 4, 5]
-- 5
last'' :: [a] -> a
last'' xs = head (reverse xs)

-- 5.
-- >>> init' [1, 2, 3, 4, 5]
-- [1,2,3,4]
init' :: [a] -> [a]
init' xs = take (length xs - 1) xs

-- >>> init'' [1, 2, 3, 4, 5]
-- [1,2,3,4]
init'' :: [a] -> [a]
init'' xs = reverse (tail (reverse xs))
