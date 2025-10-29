import Test.Tasty
import Test.Tasty.HUnit

import Lib (Mode (..), cycleLengthFold, cycleLengthInfiniteList, cycleLengthMap, cycleLengthTail, euler26, euler5Foldr, euler5InfiniteList, euler5Map, euler5Recursion, euler5Tail)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
    testGroup
        "Unit Tests"
        [ euler5Test,
          euler26Test
        ]

euler5Test :: TestTree
euler5Test =
    testGroup
        "Euler problem 5 tests"
        [ testCase "Recursion" $
            euler5Recursion 20 @?= 232792560,
          testCase "Tail recursion" $
            euler5Tail 20 @?= 232792560,
          testCase "Fold operation" $
            euler5Foldr 20 @?= 232792560,
          testCase "Map" $
            euler5Map 20 @?= 232792560,
          testCase "Infinite list" $
            euler5InfiniteList 20 @?= 232792560
        ]

euler26Test :: TestTree
euler26Test =
    testGroup
        "Euler problem 26 tests"
        [ testCase "Cycle length tail for 1/7" $
            cycleLengthTail 7 @?= 6,
          testCase "Cycle length tail for 1/3" $
            cycleLengthTail 3 @?= 1,
          testCase "Cycle length tail for 1/10" $
            cycleLengthTail 10 @?= 0,
          testCase "Cycle length map for 1/7" $
            cycleLengthMap 7 @?= 6,
          testCase "Cycle length map for 1/3" $
            cycleLengthMap 3 @?= 1,
          testCase "Cycle length map for 1/10" $
            cycleLengthMap 10 @?= 0,
          testCase "Cycle length infinite list for 1/7" $
            cycleLengthInfiniteList 7 @?= 6,
          testCase "Cycle length infinite list for 1/3" $
            cycleLengthInfiniteList 3 @?= 1,
          testCase "Cycle length infinite list for 1/10" $
            cycleLengthInfiniteList 10 @?= 0,
          testCase "Cycle length fold for 1/7" $
            cycleLengthFold 7 @?= 6,
          testCase "Cycle length fold for 1/3" $
            cycleLengthFold 3 @?= 1,
          testCase "Cycle length fold for 1/10" $
            cycleLengthFold 10 @?= 0,
          testCase "Tail recursion" $
            euler26 1000 Tail @?= 983,
          testCase "Map" $
            euler26 1000 Map @?= 983,
          testCase "Infinte list" $
            euler26 1000 InfiniteList @?= 983,
          testCase "Fold" $
            euler26 1000 Fold @?= 983
        ]
