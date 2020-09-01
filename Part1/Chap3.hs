{-# OPTIONS -Wall #-}

module Main where

-- >>> head []
-- Prelude.head: empty list

add :: (Int, Int) -> Int
add (x, y) = x + y

zeroto :: Int -> [Int]
zeroto n = [0 .. n]

-- >>> let plus3 = add' 3
-- >>> plus3 4
-- 7
add' :: Int -> (Int -> Int)
add' x y = x + y

-- >>> let mult1 = mult 2
-- >>> let mult2 = mult1 3
-- >>> mult2 4
-- 24
mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z

-- Int -> Int -> Int -> Int
-- is same as
-- Int -> (Int -> (Int -> Int))

-- mult x y z
-- is same as
-- (((mult x) y) z)

-- >>> length [1, 3, 5, 7]
-- 4

-- >>> length ["Yes", "No"]
-- 2

-- >>> length [sin, cos, tan]
-- 3

-- Eq

-- >>> False == False
-- True

-- >>> 'a' == 'b'
-- False

-- >>> "abc" == "abc"
-- True

-- >>> [1, 2] == [1, 2, 3]
-- False

-- >>> ('a', False) == ('a', False)
-- True

-- Ord

-- >>> False < True
-- True

-- >>> min 'a' 'b'
-- 'a'

-- >>> [1, 2, 3] < [1, 2]
-- False

-- >>> ('a', 2) < ('b', 1)
-- True

-- >>> ('a', 2) < ('a', 1)
-- False

-- Show

-- >>> show False
-- "False"

-- >>> show 'a'
-- "'a'"

-- >>> show 123
-- "123"

-- >>> show [1, 2, 3]
-- "[1,2,3]"

-- >>> show ('a', False)
-- "('a',False)"

-- Read

-- >>> read "False" :: Bool
-- False

-- >>> read "'a'" :: Char
-- 'a'

-- >>> read "123" :: Int
-- 123

-- >>> read "[1, 2, 3]" :: [Int]
-- [1,2,3]

-- >>> read "('a', False)" :: (Char, Bool)
-- ('a',False)

-- >>> not (read "abc")
-- Prelude.read: no parse

-- Num

-- >>> 1 + 2
-- 3

-- >>> 1.0 + 2.0
-- 3.0

-- >>> negate 3.0
-- -3.0

-- >>> abs (-3)
-- 3

-- >>> signum (-3)
-- -1

-- Integral

-- >>> 7 `div` 2
-- 3

-- >>> 7 `mod` 2
-- 1

-- >>> 7.0 / 2.0
-- 3.5

-- >>> recip 2.0
-- 0.5

{- 1.3.11 -}
-- 1.
-- [Char]
-- (Char, Char, Char)
-- [(Bool, Char )]
-- ([Bool], [Char])
-- [[a] -> [a]]

-- 2.
bools :: [Bool]
bools = [True, False, True]

nums :: [[Int]]
nums = [[1, 2, 3], [1, 2, 3]]

add''' :: Int -> Int -> Int -> Int
add''' x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x

-- 3.
-- [a] -> a
-- (a, b) -> (b, a)
-- a -> b -> (a, b)
-- Num a => a -> a
-- Bool
-- (a -> a) -> a -> a

-- 5.
-- それぞれに同じ引数を与えた場合に同じ結果を返すことが型のみでは保証出来ないから。具体的な値を持つ場合は、比較が可能。
