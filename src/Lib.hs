module Lib (euler5Tail, euler5Recursion, euler5Foldr, euler5Map, euler5InfiniteList, euler26Tail, euler26Recursion, euler26Foldr, euler26Map, euler26InfiniteList, cycleLength) where

import Data.List (elemIndex)

euler5Tail :: Integer -> Integer
euler5Tail n
    | n < 1 = -1
    | otherwise = helper n 1
  where
    helper 1 acc = acc
    helper k acc = helper (k - 1) (lcm acc k)

euler5Recursion :: Integer -> Integer
euler5Recursion n
    | n < 1 = -1
    | n == 1 = 1
    | otherwise = lcm (euler5Recursion $ n - 1) n

euler5Foldr :: Integer -> Integer
euler5Foldr n
    | n < 1 = -1
    | n == 1 = 1
    | otherwise = foldr lcm 1 [1, 2 .. n]

euler5Map :: Integer -> Integer
euler5Map n
    | n < 1 = -1
    | n == 1 = 1
    | otherwise = case helper n of
        (h : _) -> h
        [] -> -1
  where
    helper 1 = [1]
    helper k = map (lcm k) (helper $ k - 1)

euler5InfiniteList :: Integer -> Integer
euler5InfiniteList n
    | n < 1 = -1
    | n == 1 = 1
    | otherwise = case helper n [1 ..] of
        (h : _) -> h
        [] -> -1
  where
    helper _ [] = []
    helper _ [_] = []
    helper 1 list = list
    helper k (x1 : x2 : xs) = helper (k - 1) (lcm x1 x2 : xs)

cycleLength :: Integer -> Integer
cycleLength n = case helper 10 [] of
    Just result -> toInteger result
    Nothing -> -1
  where
    helper base rems
        | base `mod` n == 0 = Just 0
        | (base `mod` n) `elem` rems = elemIndex (base `mod` n) (0 : rems)
        | otherwise = helper (10 * (base `mod` n)) ((base `mod` n) : rems)

euler26Tail :: Integer -> Integer
euler26Tail = helper 0 0
  where
    helper result maxLength n
        | n == 0 = result
        | cycleLength n > maxLength = helper n (cycleLength n) (n - 1)
        | otherwise = helper result maxLength (n - 1)

euler26Recursion :: Integer -> Integer
euler26Recursion = snd . helper
  where
    helper 1 = (cycleLength 1, 1)
    helper n = max (cycleLength n, n) (helper (n - 1))

euler26Foldr :: Integer -> Integer
euler26Foldr n = snd $ foldr max (-1, 0) [(cycleLength x, x) | x <- [1 .. n]]

euler26Map :: Integer -> Integer
euler26Map n = snd . maximum $ map (\x -> (cycleLength x, x)) [1 .. n]

euler26InfiniteList :: Integer -> Integer
euler26InfiniteList n = snd . maximum $ take (fromIntegral n) (zip ([cycleLength x | x <- [1 ..]]) [1 ..])
