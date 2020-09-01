{-# OPTIONS -Wall #-}

module Main where

-- >>> double 3
-- 6

-- >>> double(double 2)
-- 8
double :: Num a => a -> a
double x = x + x

-- >>> sum [1 .. 5]
-- 15

sum' :: Num p => [p] -> p
sum' [] = 0
sum' (n : ns) = n + sum ns

-- >>> qsort [] :: [Int]
-- []

-- >>> qsort [3, 5, 1, 4, 2]
-- [1,2,3,4,5]

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [y | y <- xs, y <= x]
    larger = [y | y <- xs, y > x]

-- >>> seqn [getChar, getChar, getChar]
-- "mma"

seqn :: Monad m => [m a] -> m [a]
seqn [] = return []
seqn (a : as) = do
  x <- a
  xs <- seqn as
  return (x : xs)

{- 1.1.7 -}
-- 1.
-- double (double 2)
-- double (2 + 2)
-- (2 + 2) + (2 + 2)
-- 4 + 4
-- 8

-- 2.
-- >>> ssum [10] == (10 :: Int)
-- True
ssum :: Num a => [a] -> a
ssum [] = 0
ssum [x] = x + ssum []

-- 3.
-- >>> product' [2, 3, 4] == 24
-- True
product' :: Num p => [p] -> p
product' [] = 1
product' (x : xs) = x * (product' xs)

-- 4.
-- >>> qsort' [3, 5, 1, 4, 2]
-- [5,4,3,2,1]
qsort' :: Ord a => [a] -> [a]
qsort' [] = []
qsort' (x : xs) = qsort' larger ++ [x] ++ qsort' smaller
  where
    larger = [y | y <- xs, y >= x]
    smaller = [y | y <- xs, y < x]

-- 5.
-- [2, 2, 3, 1, 1] -> [1, 2, 3]
-- >>> qsort'' [2, 2, 3, 1, 1]
-- [1,2,3]
qsort'' :: Ord a => [a] -> [a]
qsort'' [] = []
qsort'' (x : xs) = qsort'' smaller ++ [x] ++ qsort'' larger
  where
    smaller = [y | y <- xs, y < x]
    larger = [y | y <- xs, y > x]
