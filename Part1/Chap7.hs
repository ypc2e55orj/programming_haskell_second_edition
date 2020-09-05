{-# OPTIONS -Wall #-}

module Main where

import Data.Char
import Data.List

add :: Int -> Int -> Int
add x y = x + y

add' :: Int -> (Int -> Int)
add' = \x -> \y -> x + y

-- >>> twice (*2) 3
-- 12
-- >>> twice reverse [1 .. 5]
-- [1,2,3,4,5]
twice :: (a -> a) -> a -> a
twice f a = f $ f a

-- >>> map' (+1) [1, 3, 5, 7]
-- [2,4,6,8]
-- >>> map' even [1 .. 10]
-- [False,True,False,True,False,True,False,True,False,True]
-- >>> map' reverse ["abc", "def", "ghi"]
-- ["cba","fed","ihg"]
-- >>> map' (map' (+1)) [[1, 2, 3], [4, 5]]
-- [[2,3,4],[5,6]]
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

map'' :: (a -> b) -> [a] -> [b]
map'' _ [] = []
map'' f (x : xs) = f x : map'' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' _ [] = []
filter'' p (x : xs)
  | p x = x : filter'' p xs
  | otherwise = filter'' p xs

-- >>> sumsquareeven [1 .. 10]
-- 220
sumsquareeven :: [Int] -> Int
sumsquareeven = sum . map (^ (2 :: Int)) . filter even

-- >>> all even [2, 4, 6, 8]
-- True

-- >>> any odd [2, 4, 6, 8]
-- False

-- >>> takeWhile even [2, 4, 6, 7, 8]
-- [2,4,6]

-- >>> dropWhile odd [1, 3, 5, 6, 7]
-- [6,7]

sum' :: Num a => [a] -> a
sum' = foldr (+) 0

product' :: Num a => [a] -> a
product' = foldr (*) 1

or' :: [Bool] -> Bool
or' = foldr (||) False

and' :: [Bool] -> Bool
and' = foldr (&&) True

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc [] = acc
foldr' f acc (x : xs) = f x (foldr' f acc xs)

length' :: [a] -> Int
length' = foldr (\_ acc -> 1 + acc) 0

reverse' :: [a] -> [a]
reverse' = foldr snoc []
  where
    snoc x xs = xs ++ [x]

sum'' :: Num a => [a] -> a
sum'' = foldl (+) 0

product'' :: [Int] -> Int
product'' = foldl (*) 1

(...) :: (b -> c) -> (a -> b) -> (a -> c)
f ... g = \x -> f (g x)

odd' :: Integer -> Bool
odd' = not . even

twice' :: (b -> b) -> b -> b
twice' f = f . f

sumsquareeven' :: Integral a => [a] -> a
sumsquareeven' = sum . map (^ (2 :: Int)) . filter even

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

type Bit = Int

-- >>> bin2int [1, 0, 1, 1]
-- 13
bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w, b) <- zip weight bits]
  where
    weight = iterate (* 2) 1

-- >>> bin2int' [1, 0, 1, 1]
-- 13
bin2int' :: [Bit] -> Int
bin2int' = foldr (\x acc -> x + 2 * acc) 0

-- >>> int2bin 13
-- [1,0,1,1]
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- >>> make8 [1, 0, 1, 1]
-- [1,0,1,1,0,0,0,0]
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- >>> encode "abc"
-- [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- >>> decode $ encode "abc"
-- "abc"
decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

-- >>> transmit "higher-order functions are easy"
-- "higher-order functions are easy"
transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

-- >>> count "Red" votes
-- 2
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- >>> rmdups votes
-- ["Red","Blue","Green"]
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

-- >>> result votes
-- [(1,"Green"),(2,"Red"),(3,"Blue")]
result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

-- >>> winner votes
-- "Blue"
winner :: Ord a => [a] -> a
winner = snd . last . result

ballots :: [[String]]
ballots =
  [ ["Red", "Green"],
    ["Blue"],
    ["Green", "Red", "Blue"],
    ["Blue", "Green", "Red"],
    ["Green"]
  ]

-- >>> rmempty ballots
-- [["Red","Green"],["Blue"],["Green","Red","Blue"],["Blue","Green","Red"],["Green"]]
rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

-- >>> let first = elim "Red" ballots
-- >>> let second = elim "Blue" first
-- >>> let third = rmempty second
-- >>> rank third
-- ["Green"]
elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

-- >>> result . map head $ ballots
-- [(1,"Red"),(2,"Blue"),(2,"Green")]
-- >>> rank ballots
-- ["Red","Blue","Green"]
rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

-- >>> winner' ballots
-- "Green"
winner' :: Ord a => [[a]] -> a
winner' bs = case rank $ rmempty bs of
  [c] -> c
  (c : _) -> winner' (elim c bs)

{- 1.7.9 -}
-- 1.
-- >>> one (+2) even [1..10]
-- [4,6,8,10,12]
one :: (a -> b) -> (a -> Bool) -> [a] -> [b]
one f p xs = map f . filter p $ xs

-- 2.
-- a
-- >>> all' id [True, False, True]
-- False
-- >>> all id [True, False, True]
-- False
all' :: (a -> Bool) -> [a] -> Bool
all' f = and . map f

-- b
-- >>> any' id [True, False, True]
-- True
-- >>> any id [True, False, True]
-- True
any' :: (a -> Bool) -> [a] -> Bool
any' f = or . map f

-- c
-- >>> takeWhile' (<5) [1 ..]
-- [1,2,3,4]
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x : xs)
  | f x = x : takeWhile' f xs
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f xa@(x : xs)
  | f x = dropWhile' f xs
  | otherwise = xa

-- 3.
-- >>> map''' (+2) [1 .. 10]
-- [3,4,5,6,7,8,9,10,11,12]
map''' :: (a -> b) -> [a] -> [b]
map''' f = foldr (\x acc -> f x : acc) []

-- >>> filter''' (<10) [1..100]
-- [1,2,3,4,5,6,7,8,9]
filter''' :: (a -> Bool) -> [a] -> [a]
filter''' f = foldr (\x acc -> if f x then x : acc else acc) []

-- 4.
-- >>> dec2int [2, 3, 4, 5]
-- 2345
dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> acc * 10 + x) 0

-- 5.
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

-- 6.
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

int2bin' :: Int -> [Int]
int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

-- >>> chop8' $ encode "abc"
-- [[1,0,0,0,0,1,1,0],[0,1,0,0,0,1,1,0],[1,1,0,0,0,1,1,0]]
chop8' :: [Bit] -> [[Bit]]
chop8' = unfold (== []) (take 8) (drop 8)

-- >>> map'''' (+1) [1 .. 10]
-- [2,3,4,5,6,7,8,9,10,11]
map'''' :: (a -> b) -> [a] -> [b]
map'''' f = unfold null (f . head) tail

-- >>> takeWhile (<10) $ iterate (+1) 0
-- [0,1,2,3,4,5,6,7,8,9]
iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f

-- 7.
parity :: [Bit] -> Bit
parity bits = if even . sum $ bits then 0 else 1

make8' :: [Bit] -> [Int]
make8' bits = parity b : b
  where
    b = take 8 (bits ++ repeat 0)

-- >>> chop8'' $ encode' "abc"
-- [[1,0,0,0,0,1,1,0],[0,1,0,0,0,1,1,0],[1,1,0,0,0,1,1,0]]
chop8'' :: [Bit] -> [[Bit]]
chop8'' [] = []
chop8'' (p : bits)
  | p == parity byte = byte : chop8'' next
  | otherwise = error "parity check error"
  where
    byte = take 8 bits
    next = drop 8 bits

-- >>> encode' "abc"
-- [1,1,0,0,0,0,1,1,0,1,0,1,0,0,0,1,1,0,0,1,1,0,0,0,1,1,0]
encode' :: String -> [Bit]
encode' = concat . map (make8' . int2bin . ord)

-- >>> decode' $ encode' "abc"
-- "abc"
decode' :: [Bit] -> String
decode' = map (chr . bin2int) . chop8''

-- >>> dropTransmit "heigher-order functions are easy"
-- parity check error
-- CallStack (from HasCallStack):
--   error, called at /home/user/Projects/programming_haskell_second_edition/Part1/Chap7.hs:316:17 in fake_uid:Main
dropTransmit :: String -> String
dropTransmit = decode' . tail . encode'

-- >>> altMap (+10) (+100) [0, 1, 2, 3, 4]
-- [10,101,12,103,14]
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f h (x : xs) = f x : altMap h f xs

luhnMinus :: Int -> Int
luhnMinus x = if x > 9 then x - 9 else x

luhnDouble :: Int -> Int
luhnDouble x = luhnMinus (x * 2)

-- >>> luhn [1, 7, 8, 4]
-- True
-- >>> luhn [4, 9, 9, 2, 7, 3, 9, 8, 7, 1, 6]
-- True
luhn :: [Int] -> Bool
luhn = (== 0) . (`mod` 10) . sum . altMap luhnMinus luhnDouble . reverse
