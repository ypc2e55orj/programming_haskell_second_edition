{-# OPTIONS -Wall #-}

module Main where

-- >>> even' 2
-- True
even' :: Integral a => a -> Bool
even' n = n `mod` 2 == 0

-- >>> splitAt 3 [1, 2, 3, 4, 5, 6]
-- ([1,2,3],[4,5,6])
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)

-- >>> recip' 2
-- 0.5
recip' :: Fractional a => a -> a
recip' n = 1 / n

-- >>> abs' (-1)
-- 1
abs' :: Int -> Int
abs' n = if n < 0 then - n else n

-- >>> signum' (-1)
-- -1
signum' :: Int -> Int
signum' n = if n < 0 then -1 else if n == 0 then 0 else 1

-- >>> abs'' (-1)
-- 1
abs'' :: Int -> Int
abs'' n
  | n < 0 = - n
  | otherwise = n

-- >>> signum'' (-1)
-- -1
signum'' :: Int -> Int
signum'' n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1

-- >>> not' True
-- False
not' :: Bool -> Bool
not' False = True
not' True = False

-- >>> True &&& True
-- True
-- >>> True &&& False
-- False
(&&&) :: Bool -> Bool -> Bool
True &&& b = b
_ &&& _ = False

-- >>> fst' (1, 2)
-- 1
fst' :: (a, b) -> a
fst' (x, _) = x

-- >>> snd' (1, 2)
-- 2
snd' :: (a, b) -> b
snd' (_, x) = x

-- >>> test ['a' , 'b', 'c']
-- True
test :: [Char] -> Bool
test ['a', _, _] = True
test _ = False

-- >>> test' ['a' , 'b', 'c']
-- True
test' :: [Char] -> Bool
test' ('a' : _) = True
test' _ = False

-- >>> head' [1 .. 5]
-- 1
head' :: [a] -> a
head' (x : _) = x

-- >>> tail' [1 .. 5]
-- [2,3,4,5]
tail' :: [a] -> [a]
tail' (_ : xs) = xs

-- >>> (\x -> x + x) 2
-- 4

-- >>> let addTwo = add' 2
-- >>> addTwo 5
-- 7
add' :: Int -> (Int -> Int)
add' = \x -> (\y -> x + y)

-- >>> let const1 = const 1
-- >>> const1 2
-- 1
const' :: a -> (b -> a)
const' x = \_ -> x

{-
odds :: Int -> [Int]
odds n = map f [0 .. n - 1]
    where f x = x * 2 + 1
-}

-- >>> odds 5
-- [1,3,5,7,9]
odds :: Int -> [Int]
odds n = map (\x -> x * 2 + 1) [0 .. n -1]

{- 1.4.8 -}
-- 1.
-- >>> halve [1..6]
-- ([1,2,3],[4,5,6])
halve :: [a] -> ([a], [a])
halve xs = (take h xs, drop h xs)
  where
    h = length xs `div` 2

-- 2.
third :: [a] -> a
third = head . tail . tail

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (_ : _ : t : _) = t

-- 3.
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs
  | null xs = []
  | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs

-- 4.
(|||) :: Bool -> Bool -> Bool
True ||| _ = True
_ ||| _ = False

(||||) :: Bool -> Bool -> Bool
False |||| False = False
False |||| True = True
True |||| False = True
True |||| True = True

(|||||) :: Bool -> Bool -> Bool
False ||||| False = False
_ ||||| _ = True

(||||||) :: Bool -> Bool -> Bool
b |||||| c
  | b == c = b
  | otherwise = True

-- 5.
(&&&&) :: Bool -> Bool -> Bool
a &&&& b = if a then if b then True else False else False

-- 6.
(&&&&&) :: Bool -> Bool -> Bool
a &&&&& b = if a then b else False

-- 7.
mult :: Int -> (Int -> (Int -> Int))
mult = \x -> (\y -> (\z -> x * y * z))

-- 8.
-- >>> luhnDouble 3
-- 6
-- >>> luhnDouble 6
-- 3
luhnDouble :: Int -> Int
luhnDouble x
  | x' > 9 = x' - 9
  | otherwise = x'
  where
    x' = x * 2

-- >>> luhn 1 7 8 4
-- True
-- >>> luhn 4 7 8 3
-- False
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b' + luhnDouble c + d') `mod` 10 == 0
  where
    d' = if d > 9 then d - 9 else d
    b' = if b > 9 then b - 9 else b
