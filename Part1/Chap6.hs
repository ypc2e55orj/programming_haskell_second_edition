{-# OPTIONS -Wall #-}

module Main where

-- >>> fac 4
-- 24
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

-- >>> product' [1 .. 5]
-- 120
product' :: Num a => [a] -> a
product' [] = 1
product' (x : xs) = x * product' xs

-- >>> length' [1 .. 5]
-- 5
length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = 1 + length' xs

-- >>> reverse' [1 .. 5]
-- [5,4,3,2,1]
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

-- >>> [1 .. 5] ++. [6 .. 10]
-- [1,2,3,4,5,6,7,8,9,10]
(++.) :: [a] -> [a] -> [a]
[] ++. ys = ys
(x : xs) ++. ys = x : (xs ++. ys)

-- >>> insert 6 [1 .. 5]
-- [1,2,3,4,5,6]
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

-- >>> isort [1..5]
-- [1,2,3,4,5]
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x : xs) = insert x (isort xs)

-- >>> zip' [1 ..] ['a' .. 'd']
-- [(1,'a'),(2,'b'),(3,'c'),(4,'d')]
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

-- >>> drop 3 [1 .. 10]
-- [4,5,6,7,8,9,10]
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_ : xs) = drop' (n - 1) xs

-- >>> fib 10
-- 55
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

-- >>> evens [0 .. 10]
-- [0,2,4,6,8,10]
evens :: [a] -> [a]
evens [] = []
evens (x : xs) = x : odds xs
  where
    odds [] = []
    odds (_ : xs') = evens xs'

-- >>> product'' [1 .. 5]
-- 120
product'' :: Num a => [a] -> a
product'' = foldr (*) 1

-- >>> drop'' 3 [0 .. 10]
-- [3,4,5,6,7,8,9,10]
drop'' :: Int -> [a] -> [a]
drop'' 0 xs = xs
drop'' _ [] = []
drop'' n (_ : xs) = drop'' (n - 1) xs

init' :: [a] -> [a]
init' [] = []
init' [_] = []
init' (x : xs) = x : init' xs

{- 1.6.8 -}
-- 1.
-- 負の整数が与えられた場合、再帰部で-1されることから、0に到達することはないために関数は停止しない。
-- >>> fac' (-1)
-- /home/user/Projects/programming_haskell_second_edition/Part1/Chap6.hs:(103,1)-(105,28): Non-exhaustive patterns in function fac'
fac' :: Int -> Int
fac' 0 = 1
fac' n
  | n > 0 = n * fac' (n - 1)

-- 2.
-- >>> sumdown 3
-- 6
sumdown :: (Eq p, Num p) => p -> p
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- 3.
-- >>> 2 ^. 3
-- 8
-- 2 ^. 3
-- 2 * (2 ^. (3 - 1))
-- 2 * (2 * (2 ^. (2 - 1)))
-- 2 * (2 * (2 * (2 ^. (1 - 1))))
-- 2 * (2 * (2 * 1))
(^.) :: Int -> Int -> Int
0 ^. _ = 0
_ ^. 0 = 1
x ^. n = x * (x ^. (n - 1))

-- 4.
-- >>> euclid 6 27
-- 3
euclid :: Int -> Int -> Int
euclid x y
  | x == y = x
  | x > y = euclid y (x - y)
  | otherwise = euclid x (y - x)

-- 5.
-- length [1, 2, 3]
-- 1 + (length [2, 3])
-- 1 + (1 + (length [3]))
-- 1 + (1 + (1 + length []))
-- 1 + (1 + (1 + 0))

-- drop 3 [1, 2, 3, 4, 5]
-- drop (3 - 1) [2, 3, 4, 5]
-- drop (2 - 1) [3, 4, 5]
-- drop (1 - 1) [4, 5]
-- [4, 5]

-- init [1, 2, 3]
-- 1 : (init [2, 3])
-- 1 : (2 : (init [3]))
-- 1 : (2 : [])

-- 6.
-- a
-- >>> and' [True, False, True]
-- False
-- >>> and' [True, True, True]
-- True
and' :: [Bool] -> Bool
and' [] = True
and' (x : xs) = x && and' xs

-- b
-- >>> concat' [[1 .. 5], [6 .. 10]]
-- [1,2,3,4,5,6,7,8,9,10]
concat' :: [[a]] -> [a]
concat' [] = []
concat' [x] = x
concat' (x : xs) = x ++ concat' xs

-- c
-- >>> replicate' 10 1
-- [1,1,1,1,1,1,1,1,1,1]
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

-- d
-- >>> [1 .. 5] !!. 4
-- 5
(!!.) :: [a] -> Int -> a
(x : _) !!. 0 = x
(_ : xs) !!. n = xs !!. (n - 1)

-- e
-- >>> elem' 'z' ['a' .. 'z']
-- True
-- >>> elem' 'z' ['a' .. 'd']
-- False
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y : ys)
  | x == y = True
  | otherwise = elem' x ys

-- 7.
-- >>> merge [2, 5, 6] [1, 3, 4]
-- [1,2,3,4,5,6]
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xa@(x : xs) ya@(y : ys)
  | x <= y = x : merge xs ya
  | otherwise = y : merge xa ys

-- 8.
halve :: [a] -> ([a], [a])
halve xs = (take len xs, drop len xs)
  where
    len = length xs `div` 2

-- >>> msort [1, 9, 2, 8, 3, 7, 4, 6, 5]
-- [1,2,3,4,5,6,7,8,9]
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort l) (msort r)
  where
    (l, r) = halve xs

-- 9.
-- a.
-- >>> sum' [1 .. 10]
-- 55
sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum' xs

-- b.
-- >>> take' 10 [1 ..]
-- [1,2,3,4,5,6,7,8,9,10]
take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x : xs) = x : take' (n - 1) xs

-- c.
-- >>> last' [1 .. 10]
-- 10
last' :: [a] -> a
last' [x] = x
last' (_ : xs) = last' xs
