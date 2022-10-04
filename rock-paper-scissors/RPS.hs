{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RPS where

import Data.Char
import Data.Int
import GHC.Generics
import GHC.Stack
import System.Random
import System.Random.Internal

data RPS = Rock | Paper | Scissors
    deriving (Eq, Show, Enum, Generic, Uniform, Read)

instance Ord RPS where
    compare Rock Scissors = GT
    compare Scissors Rock = LT
    compare a b = compare (fromEnum a) (fromEnum b)

fromString :: HasCallStack => String -> RPS
fromString s = case map toLower s of
    'p' : _ -> Paper
    'r' : _ -> Rock
    's' : _ -> Scissors
    _ -> error "what"

winVerb Rock = "smashes"
winVerb Paper = "wraps"
winVerb Scissors = "cuts"

{- instance Uniform RPS where
                         -- uniformM :: StatefulGen g m => g -> m a
                         uniformM g = do
                             i :: Int8 <- uniformM g -- 85 * 3 + 1 = 256
                             case i `mod` 3 of -- Deech points out hardwired constant here
                                 0 | i == 0 -> uniformM g -- Try again
                                 n -> toEnum n
                     -}