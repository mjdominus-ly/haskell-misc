import Argv
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import Text.Printf

main :: IO ()
main = do
  text <- input
  putReport $ histogram (words text)

report :: (String, Int) -> String
report (k, count) = printf "%5d" count ++ " " ++ k

type Histogram k = Map.Map k Int

putReport :: Histogram String -> IO ()
putReport = mapM_ (putStrLn . report) . sortOn (Down . snd) . Map.toList

-- try using unionsWith and a suitable monoid/semigroup for the values
histogram :: (Ord k, Foldable t) => t k -> Histogram k
histogram =
  foldr (Map.alter incr) Map.empty
 where
  incr Nothing = Just 1
  incr (Just n) = Just (n + 1)