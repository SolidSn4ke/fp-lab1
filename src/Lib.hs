module Lib (cycleLengthFold, euler5Tail, euler5Recursion, euler5Foldr, euler5Map, euler5InfiniteList, euler26, cycleLengthTail, cycleLengthInfiniteList, cycleLengthMap, Mode (..)) where

import Data.List (elemIndex, sortBy)
import Data.Ord (Down (..), comparing)

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

cycleLengthTail :: Integer -> Integer
cycleLengthTail n = case helper 10 [] of
    Just result -> toInteger result
    Nothing -> -1
  where
    helper base rems
        | base `mod` n == 0 = Just 0
        | (base `mod` n) `elem` rems = elemIndex (base `mod` n) (0 : rems)
        | otherwise = helper (10 * (base `mod` n)) ((base `mod` n) : rems)

cycleLengthInfiniteList :: Integer -> Integer
cycleLengthInfiniteList n = case helper rems [] of
    Just result -> toInteger result
    Nothing -> -1
  where
    rems = drop 1 $ iterate (\x -> x * 10 `mod` n) 1
    helper [] _ = Nothing
    helper (x : xs) seen
        | x == 0 = Just 0
        | x `elem` seen = elemIndex x (0 : seen)
        | otherwise = helper xs (x : seen)

cycleLengthMap :: Integer -> Integer
cycleLengthMap n = helper result
  where
    helper (x1 : x2 : _)
        | (\(_, _, value) -> value) x1 == 0 = 0
        | otherwise = (\(_, pos1, _) -> pos1) x1 - (\(_, pos2, _) -> pos2) x2
    helper [] = -1
    helper (_ : _) = 0
    result = take 2 $ sortBy (comparing Down) $ map (\(pos, r) -> (countElem r :: Integer, pos, r)) rems
    rems = zip [1 .. n] $ take (fromIntegral n) . drop 1 $ iterate (\x -> x * 10 `mod` n) 1
    countElem e = fromIntegral . length . filter (\(_, num) -> num == e) $ rems

cycleLengthFold :: Integer -> Integer
cycleLengthFold n = helper
  where
    helper = if 0 `elem` rems then 0 else toInteger $ distinctCount - indexOfDuplicate
    rems = take (fromIntegral n) . drop 1 $ iterate (\x -> x * 10 `mod` n) 1
    distinctCount = length $ foldl (\list e -> if e `elem` list then list else e : list) [] rems
    firstDuplicate = rems !! distinctCount
    indexOfDuplicate = case elemIndex firstDuplicate rems of
        Just index -> index
        Nothing -> -1

data Mode = Tail | Map | InfiniteList | Fold deriving (Eq)

euler26 :: Integer -> Mode -> Integer
euler26 n m
    | m == Tail = snd $ foldr max (-1, 0) [(cycleLengthTail x, x) | x <- [1 .. n]]
    | m == Map = snd $ foldr max (-1, 0) [(cycleLengthMap x, x) | x <- [1 .. n]]
    | m == InfiniteList = snd $ foldr max (-1, 0) [(cycleLengthInfiniteList x, x) | x <- [1 .. n]]
    | m == Fold = snd $ foldr max (-1, 0) [(cycleLengthFold x, x) | x <- [1 .. n]]
    | otherwise = -1
