import Test.Tasty
import Test.Tasty.HUnit

import Lib (cycleLength, euler26Foldr, euler26InfiniteList, euler26Map, euler26Recursion, euler26Tail, euler5Foldr, euler5InfiniteList, euler5Map, euler5Recursion, euler5Tail)

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
        [ testCase "Cycle length for 1/7" $
            cycleLength 7 @?= 6,
          testCase "Cycle length for 1/3" $
            cycleLength 3 @?= 1,
          testCase "Cycle length for 1/10" $
            cycleLength 10 @?= 0,
          testCase "Tail recursion" $
            euler26Tail 1000 @?= 983,
          testCase "Recursion" $
            euler26Recursion 1000 @?= 983,
          testCase "Fold" $
            euler26Foldr 1000 @?= 983,
          testCase "Map" $
            euler26Map 1000 @?= 983,
          testCase "Infinte list" $
            euler26InfiniteList 1000 @?= 983
        ]
