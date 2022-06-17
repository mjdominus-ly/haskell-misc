{-# LANGUAGE ScopedTypeVariables #-}
import Debug.Trace
import Test.QuickCheck hiding (sample)
import Test.Tasty
import Test.Tasty.HUnit
import MJDAssocMap

tests = testCase "property" $ do
        res <- quickCheckResult $ \(k,v) ->
          (find "" k (insert k v (empty :: Map Int String))) == v
        print res


main :: IO ()
main = defaultMain tests