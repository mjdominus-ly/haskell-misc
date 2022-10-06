{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RPS (fromString, winVerb, RPS) where

import Data.Char
import Data.Int
import GHC.Generics
import System.Random

data RPS = Rock | Paper | Scissors
    deriving (Eq, Show, Enum, Generic, Uniform, Read)

instance Ord RPS where
    compare Rock Scissors = GT
    compare Scissors Rock = LT
    compare a b = compare (fromEnum a) (fromEnum b)

fromString :: String -> Maybe RPS
fromString s = case map toLower s of
    'p' : _ -> return Paper
    'r' : _ -> return Rock
    's' : _ -> return Scissors
    _ -> Nothing

winVerb Rock = "smashes"
winVerb Paper = "wraps"
winVerb Scissors = "cuts"
