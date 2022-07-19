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
            length empty @?= 0
            size empty @?= 0
            (not . isEmpty) (singleton undefined undefined) @? "singleton shouldn't be empty"
            (not . isEmpty) sample @? "example isn't empty"
        , testCase "singleton" $ do
            (length $ singleton undefined undefined) @?= 1
            (size $ singleton undefined undefined) @?= 1
            fromList [(17, 23)] @?= singleton 17 23
            (member 17 $ singleton 17 23) @? "should find key in singleton map"
            (not $ member 17 $ singleton 18 23) @? "shouldn't find missing key in singleton map"
            getKeys (singleton 17 23) @?= [17]
            getValues (singleton 17 23) @?= [23]
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
