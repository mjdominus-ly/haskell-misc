{-# LANGUAGE ScopedTypeVariables #-}

module RPS where

import Data.Char
import Data.Int
import System.Random
import System.Random.Internal

data RPS = Rock | Paper | Scissors
    deriving (Eq, Show, Enum)

instance Ord RPS where
    compare Rock Scissors = GT
    compare Scissors Rock = LT
    compare a b = compare (fromEnum a) (fromEnum b)

fromString :: String -> RPS
fromString s = case map toLower s of
    'p' : _ -> Paper
    'r' : _ -> Rock
    's' : _ -> Scissors
    _ -> undefined

instance Uniform RPS where
    -- uniformM :: StatefulGen g m => g -> m a
    uniformM g = do
        i :: Int8 <- uniformM g -- 85 * 3 + 1 = 256
        case i `mod` 3 of
            0 | i == 0 -> uniformM g -- Try again
            0 -> return Rock
            1 -> return Paper
            2 -> return Scissors
