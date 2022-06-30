{-# LANGUAGE FlexibleInstances #-}

module MJDVectorMap where

import Control.Monad (join)
import Data.Char (ord)
import Data.Maybe
import Data.Monoid as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Base (undefined)
import Prelude hiding (splitAt)

-- Eventually (when we do `delete`) we'll need a three-headed type
-- that includes tombstones, but not yet
newtype Map k v
    = Map (Vector (Maybe (k, v)))
    deriving (Show)

unMap (Map x) = x

class MJDHashable t where
    hash :: t -> Int

instance MJDHashable Char where
    hash = ord

-- M.Sum isn't the right thing to use here, I just did it because it was as simple as possible
instance (Foldable f, MJDHashable a) => MJDHashable (f a) where
    -- hash as = getSum $ foldMap (M.Sum . hash) as
    hash as = 0

-- Make a new vector that is like the old one, but starting at
-- a different place and wrapping around
wrapV :: Int -> Vector a -> Vector (Int, a)
wrapV start vec = back V.++ front
  where
    (front, back) = V.splitAt start (V.indexed vec)

-- Fold a Vector, but start in the middle somewhere
-- and wrap around from the end to the beginning
-- (not currently used)
{- wrapFoldr :: (Maybe (k, v) -> b -> b) -> b -> Int -> Map k v -> b
wrapFoldr f init startPos (Map v) =
    V.foldr f init (wrapV startPos v) -}

type Slot k v = Maybe (k, v)

getIndex :: (Eq k, MJDHashable k) => k -> Map k v -> Maybe Int
getIndex k m = fmap fst $ V.find rightSlot (wrapV start $ unMap m)
  where
    start = hash k `mod` size m
    -- Stop searching if we find an empty slot...
    rightSlot (_, Nothing) = True
    -- ... or if we find a full slot with a matching key
    rightSlot (_, Just (k', _)) = k' == k

get :: (Eq k, MJDHashable k) => k -> Map k v -> Maybe v
get k m = fmap snd $ join $ ((unMap m) V.!) <$> i
  where
    i = getIndex k m

sample :: Map String String
sample = Map $ V.fromList [Just ("apple", "red"), Just ("banana", "yellow"), Nothing, Just ("kiwi", "brown")]

{- findIndex k m = (wrappedIndex + (size m))
    where
    wrappedIndex = (join . V.find rightPair) (wrapV start $ unMap m)
    start = hash k `mod` size m
    -- Stop searching if we find an empty slot...
    rightPair Nothing = True
    -- ... or if we find a full slot with a matching key
    rightPair (Just (k', _)) = k' == k -}

-- for later: destructive update here?
-- ways to do this:
--   1. implement a version of `get` that, instead of returning the value,
--      returns the index at which it was found.  This is tricky because of the way
--      you implemented `wrapV`; it doesn't know the indices any more
--   2. map over an initial segment of the whole vector, updating only the one
--      matching element, and use `V.concat` to reuse the tail of the vector

{- insert :: k -> v -> Map k v -> Map k v
insert k v m =
    undefined
  where
    start = hash k `mod` size m
    -- update if the slot is empty
    newPair Nothing = Just (k, v)
    -- ... or overwrite if we find a full slot with a matching key
    newPair (Just (k', _)) | k' == k = Just (k, v) -}

--

lift f = Map . f . unMap

-- instance Foldable (Map k) where  -- Why doesn't this work?
--    foldMap f (Map v) = foldMap f v

size :: Map k v -> Int
size = V.length . unMap
