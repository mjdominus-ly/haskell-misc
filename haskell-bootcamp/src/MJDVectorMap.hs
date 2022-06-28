module MJDVectorMap where

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

-- Apply some operation to a vector, but starting at
-- point `start` and wrapping around
-- Alternative implementation:
--      take (size m) $ drop start $ cycle m
-- wrapMap :: (a -> b) -> Int -> Map k a -> Map k b
-- wrapMap f start h = (fmap f back) ++ (fmap f front)
wrapV :: Int -> Vector a -> Vector a
wrapV start vec = back V.++ front
  where
    (front, back) = V.splitAt start vec

type KVP k v = Maybe (k, v)

wrapFold :: (b -> KVP k v -> b) -> b -> Int -> Map k v -> b
wrapFold f init startPos (Map v) =
    V.foldl f init (wrapV startPos v)

get :: (Eq k, MJDHashable k) => k -> Map k v -> Maybe v
get k m = fmap snd $ (V.find rightKey (wrapV hashVal $ unMap m) :: _)
  where
    hashVal = hash k
    rightKey Nothing = True
    rightKey (Just (k', _)) = k' == k

lift f = Map . f . unMap

{- instance Foldable (Map k) where
    foldMap f map = lift (foldMap f) -}
