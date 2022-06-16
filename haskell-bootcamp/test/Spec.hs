
import Test.Tasty
import Test.Tasty.HUnit
import MJDAssocMap

main :: IO ()
main = defaultMain tests

tests = testGroup "???" [
    testCase "empty / isEmpty" $ do
        isEmpty empty @? "isEmpty empty is false"
        (not . isEmpty) (singleton 1 2) @? "singleton shouldn't be empty" 
  ]

testsDummy = testGroup "dummy group" [
    testCase "dummy test" $ do
        1 + 1 @?= 2
        2 + 2 @?= 4
  ]
