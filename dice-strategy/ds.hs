{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE GADTs #-}
-- Which is better?  A die with 321000 or one with 500000?
--
-- How much is an extra roll worth?
--
-- What is the optimal strategy for a die with 221100?

import Data.Ratio
import Text.Printf

-- newtype Range a = Range (a, a)

type NRolls = Int

type Face = Rational

type Die = [ Face ]

-- format rational as xx.xx
v :: Rational -> String
v = printf "%.2f" . (fromRational :: Rational -> Double)

-- format rational as percentage
percentage :: Rational -> String
percentage = v . (* 100)



percentages = map percentage

sides :: Die -> Int
sides = length

defender :: Die
defender = [ 3, 2, 1, 0, 0, 0 ]

ruffian :: Die
ruffian = [ 1, 0, 0, 0, 0, 0 ]

mage :: Die
mage = [ 2, 2, 1, 0, 0, 0 ]

-- expected value of one roll of the die
xVal :: Die -> Rational
xVal = mean

mean :: (Foldable t, Fractional a) => t a -> a
mean ls = sum ls / fromIntegral (length ls)

-- a move in the game is to Stop and accept the current roll,
-- or to Go: reroll and play another strategy from that point on
data Move = Stop | Go Strat

-- a strategy is a function that takes the current face showing
-- and decides whether to Stop (accpeting it) or Go (reroll)
data Strat = Strat String (Face -> Move)
nameOf (Strat s _) = s

instance Show Strat where
  show (Strat n _) = "<strategy '" ++ n ++ "'>"

cons :: String -> (Face -> Bool) -> (Strat -> Strat)
cons pDesc p s = Strat newname (\x -> if p x then Stop else Go s)
  where newname = pDesc ++ ", otherwise roll and " ++ nameOf s

stopName n = "stop if face >= " ++  numeral
  where
   numeral = if (isInteger n) then show (floor n)
                  else show n
   isInteger x = (fromIntegral . floor) x == x

apply :: Strat -> Face -> Move
apply (Strat _ s) = s

stop :: Strat
stop = Strat "stop" (const Stop)

stopOn n = cons (stopName n) (>= n)

alwaysStopOn :: Face -> Strat
alwaysStopOn n = loop
   where loop = stopOn n loop
    
alwaysStopOn1 :: Strat
alwaysStopOn1 = alwaysStopOn 1
alwaysStopOn2 :: Strat
alwaysStopOn2 = alwaysStopOn 2

-- idea to pursue: a strategy notinally turns a die and a number of rolls into a different die.
-- for example, the die [2, 2, 1, 0, 0, 0] has expected value 5/6.
-- the stopOn2 strategy with one reroll turns this into [2, 2, 5/6, 5/6, 5/6, 5/6]
-- the stopOn1 strategy with one reroll turns it   into [2, 2, 1, 5/6, 5/6, 5/6].


-- Die -> number of rerolls -> expected value
-- this is way slower than it should be; I think some memoization
-- is missing somewhere.  
stratValue :: Strat -> Die -> NRolls -> Rational
stratValue s d 0 = xVal d
stratValue s d n = xVal fakeDie where
  fakeDie = [faceValue f | f <- d ] where
     faceValue f = case apply s f of Stop -> f -- stop and accept roll of f
                                     Go s'-> stratValue s' d (n-1) -- reroll and proceed with strategy s'

newtype Fst a b = Fst (a, b) deriving Show

instance Eq a => Eq (Fst a b) where
  Fst (l, _) == Fst (r, _)  =    l == r
instance Ord a => Ord (Fst a b) where
  compare (Fst (l, _)) (Fst (r, _)) = compare l r

-- note this returns an _a_, not a _b_.
-- if it were returning a _b_ we would just use (maximum . map f).
maximumBy :: (Ord b, Foldable t, Functor t) => (a -> b) -> t a -> a
maximumBy f ls = y
  where
    Fst (_, y) = maximum $ fmap (\x -> Fst (f x, x)) ls


-- only if you consider "optimal" to be "maximal expected value"
optimalStrategy :: Die -> NRolls -> Strat
optimalStrategy d 0 = stop
optimalStrategy d n = best [ stopOn k (optimalStrategy d (n-1)) | k <- [1,2, 3]]
  where
    best = maximumBy value
    value s = stratValue s d n