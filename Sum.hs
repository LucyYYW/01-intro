module Sum where

import Prelude hiding (foldl, foldr)

sum1 :: [Int] -> Int
sum1 [] = 0
sum1 (x : xs) = x + sum1 xs

sum2 :: [Int] -> Int
sum2 [] = 0
sum2 (x : xs) = sum2 xs + x

sum3 :: [Int] -> Int
sum3 = sumAux 0
  where
    sumAux acc [] = acc
    sumAux acc (x : xs) = sumAux (acc + x) xs

sum4 :: [Int] -> Int
sum4 = sumAux 0
  where
    sumAux acc [] = acc
    sumAux acc (x : xs) = sumAux (x + acc) xs

sum5 :: [Int] -> Int
sum5 = sumAux 0
  where
    sumAux acc [] = acc
    sumAux acc (x : xs) = (sumAux $! (x + acc)) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b = go
  where
    go [] = b
    go (x : xs) = x `f` go xs

foldrFlip :: (b -> a -> b) -> b -> [a] -> b
foldrFlip f b = go
  where
    go [] = b
    go (x : xs) = go xs `f` x

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f = go
  where
    go acc [] = acc
    go acc (x : xs) = go (acc `f` x) xs

foldlFlip :: (a -> b -> b) -> b -> [a] -> b
foldlFlip f = go
  where
    go acc [] = acc
    go acc (x : xs) = go (x `f` acc) xs

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f = go
  where
    go acc [] = acc
    go acc (x : xs) = (go $! acc `f` x) xs

h1 :: Int -> Int
h1 y = sum1 [1 .. y * 100000]

f1 :: Int -> Int -> Int
f1 _x y = sum1 [1 .. y * 100000]

g1 :: Int -> Int -> Int
g1 x y = if x > 0 then sum1 [1 .. y * 100000] else sum2 [1 .. y * 100000]

u1 :: Int
u1 = h1 1 + h1 1 + h1 1

v1 :: Int
v1 = f1 0 1 + f1 1 1 + f1 2 1

v2 :: Int
v2 = f1 0 1 + f1 0 1 + f1 0 1

w1 :: Int
w1 = g1 0 1 + g1 1 1 + g1 2 1

w2 :: Int
w2 = g1 0 1 + g1 0 1 + g1 0 1
