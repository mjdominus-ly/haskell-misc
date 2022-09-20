{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE GADTs #-}
-- Which is better?  A die with 321000 or one with 500000?
--
-- How much is an extra roll worth?
--
-- What is the optimal strategy for a die with 221100?

import Data.Ratio
import Text.Printf

type NRolls = Int

type Face = Rational

type Die = [ Face ]

percentage :: Rational -> String
percentage = printf "%.2f". (* r100) . fromRational
  where r100 = 100.0 :: Double

percentages = map percentage

sides :: Die -> Int
sides = length

defender :: Die
defender = [ 3, 2, 1, 0, 0, 0 ]

ruffian :: Die
ruffian = [ 1, 0, 0, 0, 0, 0 ]

mage :: Die
mage = [ 2, 2, 1, 1, 0, 0 ]

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

instance Show Strat where
  show (Strat n _) = "<strategy '" ++ n ++ "'>"


apply :: Strat -> Face -> Move
apply (Strat _ s) = s

stop :: Strat
stop = Strat "stop" (const Stop)

alwaysStopOn :: Face -> Strat
alwaysStopOn n = Strat name (\x -> if x >= n then Stop else Go $ alwaysStopOn n)
  where
    name = "always stop on " ++  numeral
    numeral = if (isInteger n) then show (floor n)
                  else show n
    isInteger x = (fromIntegral . floor) x == x
alwaysStopOn1 :: Strat
alwaysStopOn1 = stopOn 1
alwaysStopOn2 :: Strat
alwaysStopOn2 = stopOn 2

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

optimalStrategy :: Die -> NRolls -> Strat
optimalStrategy = undefined