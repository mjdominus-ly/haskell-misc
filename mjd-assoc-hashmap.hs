{-|
  In this module, implement Map. `Map k v` will be a map from keys of type `k` to
  values of type `v`. You can choose whatever implementation you want, here are some
  options:
  * Lookup map (store as list of (k, v) pairs and to lookup a key `k`, find the pair
     with the same `k`)
  * Binary search tree
  * Hash map

  I recommend implementing a lookup map first, but if you're looking for a challenge,
  feel free to try to implement the other ones.

  Depending on your implementation, you'll probably need to add constraints to some of
  the functions below. You'll know you need to add them if the compiler shows error
  messages. Feel free to message me if you'd like help debugging the error messages.

  Useful resources:
  * https://hoogle.haskell.org/
-}

import qualified Data.List (find)
import Data.Maybe (fromMaybe)

-- | Take a function on values and turn it into a function on kvps
_liftVal :: (t -> b) -> (a, t) -> (a, b)
_liftVal f (a, b) = (a, f b) -- could use `fmap` here but that would be more confusing

newtype Map k v = 
  Map [(k, v)]
  deriving (Show)

items :: Map k v -> [(k, v)]
items (Map ls) = ls

wrapped :: ([(k1, v1)] -> [(k2, v2)]) -> Map k1 v1 -> Map k2 v2
wrapped f = Map . f . items

key :: (k, v) -> k
key = fst

val :: (k, v) -> v
val = snd

vals :: Map k v -> [v]
vals = map val . items -- XXX eliminate in favor of getVals

instance Functor (Map k) where
  -- fmap :: (a -> b) -> Map k a -> Map k b
  fmap f = wrapped $ fmap (_liftVal f)

{- Creating a Map -}

-- | Create an empty Map.
empty :: Map k v
empty = Map []

-- | Create a Map with a single element.
singleton :: k -> v -> Map k v
singleton k v = Map [(k, v)]

-- | Create a Map from a list.
fromList :: [(k, v)] -> Map k v
fromList = Map

{- Querying a Map -}

-- \ Return True if the given Map is empty.
isEmpty :: Map k v -> Bool
isEmpty = null -- from Foldable

-- | Return the number of elements in the given Map.
size :: Map k v -> Int
size = length -- from Foldable

-- | Return True if the given key is in the Map.
member :: Eq k => k -> Map k v -> Bool
member k = elem k . getKeys

-- | Get the value for the given key in the Map.
get :: Eq k => k -> Map k v -> Maybe v
get k = fmap val . Data.List.find keyMatches . items
  where keyMatches = (== k) . key

-- | Get the value for the given key in the Map, providing a default if it does not exist.
find :: Eq k => v -> k -> Map k v -> v
find v k = fromMaybe v . get k

{- Deconstructing a Map -}

-- | Create a list from a Map.
toList :: Map k v -> [(k, v)]
toList = items

-- | Get the keys in the map.
getKeys :: Map k v -> [k]
getKeys = map key . items 

-- | Get the values in the map.
getValues :: Map k v -> [v]
getValues = map val . items

{- Manipulating a Map -}

-- | Insert the given key and value into the given Map.
-- Duplicate key problems like the one in `foldMap` below
-- can be avoided by having `insert` start by doing `delete`
insert :: k -> v -> Map k v -> Map k v
insert k v = wrapped ((k, v) :)

-- | When the predicate is true of the key, replace the value
updateIf :: (k -> Bool) -> (v -> v) -> Map k v -> Map k v 
updateIf p r = wrapped $ fmap (\pr -> if (p . key) pr then _liftVal r pr else pr)

-- | Modify the given key in the given Map.
update :: Eq k => (v -> v) -> k -> Map k v -> Map k v
update f k = updateIf (== k) f

sample :: Num a => Map a a
sample = Map [ (1,1), (2, 4), (3, 9), (-3, 9)]

-- | Delete the given key from the given Map.
delete :: Eq k => k -> Map k v -> Map k v
delete k = wrapped $ filter ((/= k) . key) -- note that it's not enough to just delete the first match

instance (Eq k, Eq v) => Eq (Map k v) where
  Map [] == Map []   = True
  Map [] == Map _    = False
  Map _  == Map []   = False
  m1@(Map ((k,v):_)) == m2 =
    get k m2 == Just v && delete k m1 == delete k m2

-- This isn't really right.  sum $ Map [(1, 3), (1, 4)]  ought to yield 3, not 7.
-- Fix later.
instance Foldable (Map k) where
  foldMap f = foldMap f . getValues
  