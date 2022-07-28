{-# LANGUAGE ScopedTypeVariables #-}

import Debug.Trace
import MJDVectorMap
import Test.QuickCheck hiding (sample)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests =
    testGroup
        "basic tests"
        [ testCase "empty / isEmpty" $ do
            isEmpty empty @? "isEmpty empty is false"
            -- length empty @?= 0
            size empty @?= 0
            let sExample = singleton (17 :: Int) (23 :: Int)
            (not . isEmpty) sExample @? "singleton shouldn't be empty"
            (not . isEmpty) sample @? "example isn't empty"
        , testCase "singleton" $ do
            let flExample = fromList [(17, 23) :: (Int, Int)]
            let sExample = singleton (17 :: Int) (23 :: Int)
            -- (length $ sExample) @?= 1
            (size $ sExample) @?= 1
            flExample @?= sExample
            (member 17 $ sExample) @? "should find key in singleton map"
            (not $ member 17 $ sExample) @? "shouldn't find missing key in singleton map"
            getKeys sExample @?= [17]
            getValues sExample @?= [23]
        , testCase "property" $
            quickCheck $ \(k, v) ->
                let m = (empty :: Map Int String)
                 in traceShow (k, v) $ (find "" k (insert k v m)) == v
        ]

testsDummy =
    testGroup
        "dummy group"
        [ testCase "dummy test" $ do
            1 + 1 @?= 2
            2 + 2 @?= 4
        ]
