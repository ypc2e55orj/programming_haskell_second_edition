{-# OPTIONS -Wall #-}

module Main where

import Data.Char

-- >>> [x^2 | x <- [1 .. 5]]
-- [1,4,9,16,25]

-- >>> [(x, y) | x <- [1, 2, 3], y <- [4, 5]]
-- [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

-- >>> [(x, y) | y <- [4, 5], x <- [1, 2, 3]]
-- [(1,4),(2,4),(3,4),(1,5),(2,5),(3,5)]

-- >>> concat [[1 .. 5], [6 .. 10]]
-- [1,2,3,4,5,6,7,8,9,10]

-- >>> factors 15
-- [1,3,5,15]
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

-- >>> prime 12
-- False
-- >>> prime 13
-- True
prime :: Int -> Bool
prime n = factors n == [1, n]

-- >>> find 5 $ zip [1..5] ['a'..]
-- "e"
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k' == k]

-- >>> pairs [1 .. 5]
-- [(1,2),(2,3),(3,4),(4,5)]
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-- >>> sorted [1..5]
-- True
-- >>> sorted [5, 3, 2, 1]
-- False
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

-- >>> positions 5 [1..10]
-- [4]
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

-- >>> lowers "Haskell"
-- 6
lowers :: String -> Int
lowers xs = length [x | x <- xs, 'a' <= x && x <= 'z']

-- >>> count 's' "Mississippi"
-- 4
count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

-- >>> let2int 'a'
-- 0
let2int :: Char -> Int
let2int c = ord c - ord 'a'

-- >>> int2let 0
-- 'a'
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

-- >>> shift 3 'a'
-- 'd'
-- >>> shift 3 'z'
-- 'c'
-- >>> shift (-3) 'c'
-- 'z'
-- >>> shift 3 ' '
-- ' '
shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

-- >>> encode 3 "haskell is fun"
-- "kdvnhoo lv ixq"
-- >>> encode (-3) "kdvnhoo lv ixq"
-- "haskell is fun"
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table =
  [ 8.1, -- a
    1.5, -- b
    2.8, -- c
    4.2, -- d
    12.7, -- e
    2.2, -- f
    2.0, -- g
    6.1, -- h
    7.0, -- i
    0.2, -- j
    0.8, -- k
    4.0, -- l
    2.4, -- m
    6.7, -- n
    7.5, -- o
    1.9, -- p
    0.1, -- q
    6.0, -- r
    6.3, -- s
    9.0, -- t
    2.8, -- u
    1.0, -- v
    2.4, -- w
    0.2, -- x
    2.0, -- y
    0.1 -- z
  ]

-- >>> percent 5 15
-- 33.333336
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

-- >>> freqs "abbcccddddeeeee"
-- [6.666667,13.333334,20.0,26.666668,33.333336,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
freqs :: String -> [Float]
freqs xs = [percent (count x xs) len | x <- ['a' .. 'z']]
  where
    len = lowers xs

chispqr :: [Float] -> [Float] -> Float
chispqr os es = sum [((o - e) ^ (2 :: Int)) / e | (o, e) <- zip os es]

-- >>> rotate 3 [1 .. 5]
-- [4,5,1,2,3]
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- >>> crack "kdvnhoo lv ixq"
-- "haskell is fun"
crack :: String -> String
crack xs = encode (- factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chispqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs

{- 1.5.7 -}
-- 1.
-- >>> sum [x^(2 :: Int) | x <- [1 .. 100]]
-- 338350

-- 2.
-- >>> grid 1 2
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

-- 3.
-- >>> square 2
-- [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 4.
-- >>> replicate' 3 True
-- [True,True,True]
replicate' :: (Num t, Enum t) => t -> a -> [a]
replicate' n e = [e | _ <- [1 .. n]]

-- 5.
-- >>> pyths 10
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
pyths :: Int -> [(Int, Int, Int)]
pyths n =
  [ (x, y, z)
    | x <- [1 .. n],
      y <- [1 .. n],
      z <- [1 .. n],
      x ^ (2 :: Int) + y ^ (2 :: Int) == z ^ (2 :: Int)
  ]

-- 6.
-- >>> perfects 500
-- [6,28,496]
perfects :: Int -> [Int]
perfects n = [x | x <- [6 .. n], x == (p x)]
  where
    p = sum . init . factors

-- 7.
-- >>> concat [[(x, y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]

-- 8.
-- >>> positions' 'b' ['a' ..]
-- [2]
positions' :: (Eq a, Num b, Enum b) => a -> [a] -> [b]
positions' x xs = find x (zip xs [1 ..])

-- 9.
-- >>> scalarproduct [1, 2, 3] [4, 5, 6]
-- 32
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- 10.
uppers :: String -> Int
uppers xs = length [x | x <- xs, 'A' <= x && x <= 'Z']

ulet2int :: Char -> Int
ulet2int c = ord c - ord 'A'

int2ulet :: Int -> Char
int2ulet n = chr (ord 'A' + n)

-- >>> shift' 3 'a'
-- 'd'
-- >>> shift' 3 'A'
-- 'D'
shift' :: Int -> Char -> Char
shift' n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = int2ulet ((ulet2int c + n) `mod` 26)
  | otherwise = c

-- >>> encode' 3 "Haskell is fun"
-- "Kdvnhoo lv ixq"
-- >>> encode' 100 "hASKELL IS FUN"
-- "dWOGAHH EO BQJ"
encode' :: Int -> String -> String
encode' n xs = [shift' n x | x <- xs]

freqs' :: String -> ([Float], [Float])
freqs' xs =
  ( [percent (count x xs) llen | x <- ['a' .. 'z']],
    [percent (count x xs) ulen | x <- ['A' .. 'Z']]
  )
  where
    llen = lowers xs
    ulen = uppers xs

-- >>> crack' "Kdvnhoo lv ixq"
-- "Haskell is fun"
-- >>> crack' "dWOGAHH EO BQJ"
-- "hASKELL IS FUN"
crack' :: String -> String
crack' xs = encode' (- factor) xs
  where
    factor = min lfactor ufactor
    lfactor = head (positions (minimum lchitab) lchitab)
    ufactor = head (positions (minimum uchitab) uchitab)
    lchitab = [chispqr (rotate n ltable) table | n <- [0 .. 25]]
    uchitab = [chispqr (rotate n utable) table | n <- [0 .. 25]]
    (ltable, utable) = freqs' xs
