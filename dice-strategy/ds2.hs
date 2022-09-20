{-# LANGUAGE RankNTypes#-}
-- Which is better?  A die with 321000 or one with 500000?
--
-- How much is an extra roll worth?
--
-- What is the optimal strategy for a die with 221100?

import Data.Ratio

ype Face a = a

type Die a = [ Face a ]

sides :: Die a -> Int
sides = length

defender :: Die Rational
defender = [ 3, 2, 1, 0, 0, 0 ]

ruffian :: Die Rational
ruffian = [ 1, 0, 0, 0, 0, 0 ]

mage :: Die Rational
mage = [ 2, 2, 1, 1, 0, 0 ]

-- expected value of one roll of the die
xVal :: Fractional b => Die b -> b
xVal = mean

mean :: (Foldable t, Fractional a) => t a -> a
mean ls = sum ls / fromIntegral (length ls)

-- a move in the game is to Stop and accept the current roll,
-- or to Go: reroll and play another strategy from that point on
data Move a = Stop | Go (Strat a)

-- a strategy is a function that takes the current face showing
-- and decides whether to Stop (accpeting it) or Go (reroll)
data Strat a = Strat (Face a -> Move a)

apply :: Strat a -> Face a -> Move a
apply (Strat s) = s

stop :: Strat a
stop = Strat (const Stop)

stopOn :: Fractional a => Face a -> Strat a
stopOn n = Strat (\x -> if (pips x) >= n then Stop else Go $ stopOn n)
stopOn1 :: Strat a
stopOn1 = stopOn 1
stopOn2 :: Strat a
stopOn2 = stopOn 2

-- idea to pursue: a strategy notinally turns a die and a number of rolls into a different die.
-- for example, the die [2, 2, 1, 0, 0, 0] has expected value 5/6.
-- the stopOn2 strategy with one reroll turns this into [2, 2, 5/6, 5/6, 5/6, 5/6]
-- the stopOn1 strategy with one reroll turns it   into [2, 2, 1, 5/6, 5/6, 5/6].


-- Die -> number of rerolls -> expected value
stratValue :: Fractional b => Strat b -> Die b -> Integer -> b
stratValue s d 0 = xVal d
stratValue s d n = xVal fakeDie where
  fakeDie = [faceValue f | f <- d ] where
     faceValue f = case apply s f of Stop -> f -- stop and accept roll of f
                                     Go s'-> stratValue s' d (n-1) -- reroll and proceed with strategy s'
