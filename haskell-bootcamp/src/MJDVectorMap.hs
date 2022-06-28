module MJDVectorMap where

import Control.Monad (join)
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude hiding (splitAt)

-- Eventually (when we do `delete`) we'll need a three-headed type
-- that includes tombstones, but not yet
newtype Map k v
    = Map (Vector (Maybe (k, v)))
    deriving (Show)

unMap (Map x) = x

class MJDHashable t where
    hash :: t -> Int

-- Make a new vector that is like the old one, but starting at
-- a different place and wrapping around
wrapV :: Int -> Vector a -> Vector a
wrapV start vec = back V.++ front
  where
    (front, back) = V.splitAt start vec

-- Fold a Vector, but start in the middle somewhere
-- and wrap around from the end to the beginning
-- (not currently used)
wrapFoldr :: (Maybe (k, v) -> b -> b) -> b -> Int -> Map k v -> b
wrapFoldr f init startPos (Map v) =
    V.foldr f init (wrapV startPos v)

get :: (Eq k, MJDHashable k) => k -> Map k v -> Maybe v
get k m = fmap snd $ (join . V.find rightPair) (wrapV start $ unMap m)
  where
    start = hash k `mod` mapSize m
    -- Stop searching if we find an empty slot...
    rightPair Nothing = True
    -- ... or if we find a full slot with a matching key
    rightPair (Just (k', _)) = k' == k

lift f = Map . f . unMap

-- instance Foldable (Map k) where
--    foldMap f (Map v) = foldMap f v

mapSize :: Map k v -> Int
mapSize = V.length . unMap
