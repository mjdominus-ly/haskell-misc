{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module MJDVectorMap where

import Data.Char (ord)
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as VM
import GHC.Real
import GHC.Stack
import Prelude hiding (splitAt)

-- Eventually (when we do `delete`) we'll need a three-headed type
-- that includes tombstones, but not yet
data Map k v = Map
    { slots :: (Vector (Slot k v))
    , fullness :: Integer
    }
    deriving (Show)

data Slot k v = Empty | Deleted | Full (k, v)
    deriving (Eq, Show)

instance Functor (Slot k) where
    fmap :: (a -> b) -> Slot k a -> Slot k b
    fmap f (Full (k, v)) = Full (k, f v)
    fmap _ Empty = Empty
    fmap _ Deleted = Deleted

-- applies f over the values
instance Functor (Map k) where
    fmap f m@(Map{slots = vec}) = m{slots = (fmap . fmap) f vec}

-- might be simpler with 'do' notation
{- mapKeys :: (k -> k') -> Map k v -> Map k' v
mapKeys f (Map{slots = vec}) = Map $ fmap (maymap (\(k, v) -> (f k, v))) vec
  where
    maymap :: (a -> b) -> Maybe a -> Maybe b
    maymap = fmap -}

class MJDHashable t where
    hash :: t -> Int

instance MJDHashable Char where
    hash = ord

instance MJDHashable Int where
    hash = id

instance (Foldable f, MJDHashable a) => MJDHashable (f a) where
    hash = foldr (\a b -> 7 * b + (hash a)) 0

-- Make a new vector that is like the old one, but starting at
-- a different place and wrapping around
wrapV :: Int -> Vector a -> Vector (Int, a)
wrapV start vec = back V.++ front
  where
    (front, back) = V.splitAt start (V.indexed vec)

valueOf :: Slot k v -> Maybe v
valueOf (Full (_, v)) = Just v
valueOf _ = Nothing

-- look up a key in the map, return the number of the slot
-- where it was found, or where it should be inserted if it's not there yet
getIndex :: (Eq k, MJDHashable k) => k -> Map k v -> Maybe Int
getIndex k m = fmap fst $ V.find rightSlot (wrapV start $ slots m)
  where
    start = hash k `mod` size m
    -- Stop searching if we find an empty slot...
    rightSlot (_, Empty) = True
    -- ... or if we find a full slot with a matching key
    rightSlot (_, Full (k', _)) = k' == k
    -- Keep searching if we have a tombstone
    rightSlot (_, Deleted) = False

get :: (Eq k, MJDHashable k) => k -> Map k v -> Maybe v
get k m@(Map{slots = vec}) = do
    i <- getIndex k m
    valueOf $ vec V.! i

-- | Get the value for the given key in the Map, providing a default if it does not exist.
find :: (Eq k, MJDHashable k) => v -> k -> Map k v -> v
find deflt k m = fromMaybe deflt $ get k m

insert :: (HasCallStack, Eq k, MJDHashable k) => k -> v -> Map k v -> Map k v
insert k v m@(Map{slots}) =
    if density m > 0.5
        then insert k v (doubleSize m)
        else
            Map
                { slots = update1' slots i (Full (k, v))
                , fullness = 1 + fullness m -- not necessarily
                }
  where
    -- update1 vec i v = vec V.// [(i, v)]
    update1' vec i v = V.modify (\vec' -> VM.write vec' i v) vec
    i = fromMaybe err (getIndex k m)
    err = error "can't insert into full table"

insertPair :: (Eq k, MJDHashable k) => (k, v) -> Map k v -> Map k v
insertPair (k, v) m = insert k v m

doubleSize :: (Eq k, MJDHashable k) => Map k v -> Map k v
doubleSize m = fromListWithSize (2 * size m) (getItems m)

size :: Map k v -> Int
size = V.length . slots

a :: Int
a = 3

density :: Map k v -> Double
density m = (fromIntegral $ fullness m) / (fromIntegral $ size m)

emptyMap :: Int -> Map k v
emptyMap n
    | n < 4 =
        error "Minimum size for a map is 4 slots"
    | otherwise =
        Map
            { slots = V.replicate n Empty
            , fullness = 0
            }

_minSize :: Int
_minSize = 8

empty :: Map k v
empty = emptyMap _minSize

singleton :: (Eq k, MJDHashable k) => k -> v -> Map k v
singleton k v = fromListWithSize _minSize [(k, v)]

fromListWithSize :: (MJDHashable k, Eq k) => Int -> [(k, v)] -> Map k v
fromListWithSize newSize items =
    foldl
        (\m -> \(k, v) -> insert k v m)
        (emptyMap newSize)
        items

fromList :: (MJDHashable k, Eq k) => [(k, v)] -> Map k v
fromList ls = fromListWithSize (5 * length ls) ls

isEmpty :: Map k v -> Bool
isEmpty m = fullness m == 0

getItems :: Map k v -> [(k, v)]
getItems (Map{slots = vec}) = [(k, v) | Full (k, v) <- V.toList vec]

getKeys :: Map k v -> [k]
getKeys = map fst . getItems

getValues :: Map k v -> [v]
getValues = map snd . getItems

sampleKVPs :: [(String, String)]
sampleKVPs = [("potato", "blue"), ("apple", "red"), ("banana", "yellow"), ("kiwi", "brown"), ("blackberry", "black"), ("octopus", "purple")]

sample :: Map String String
sample = fromListWithSize 8 sampleKVPs

member :: (MJDHashable k, Eq k) => k -> Map k v -> Bool
member k m = isJust $ get k m

lift :: (Vector (Slot k v) -> Vector (Slot k v)) -> (Map k v -> Map k v)
lift f m@Map{slots} = m{slots = f slots}

-- instance Foldable (Map k) wher:re  -- Why doesn't this work?
--    foldMap f (Map v) = foldMap f v

-- For debugging, thanks Deech
-- important feature: if the pattern match fails, the
-- element is just skipped silently
display :: Map k v -> [(Int, k, v)]
display m = [(i, k, v) | (i, Full (k, v)) <- (V.toList . V.indexed . slots) m]

instance (Eq k, Eq v) => Eq (Map k v) where
    Map{slots = []} == Map{slots = []} = True
    Map{slots = []} == Map _ = False
    Map _ == Map{slots = []} = False
    m1@(Map{slots = ((k, v) : _)}) == m2 =
        get k m2 == Just v && delete k m1 == delete k m2