{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.IO.Class
import Data.Int

import MJDRandom
import RPSgame
import System.Random

main :: IO ()
main = do
    rng <- initStdGen
    let (p1, p2) = (makeComputerPlayer 1, makeHumanPlayer "Alice")
    winner <- evalRandT (makeGame p1 p2) rng
    putStrLn $ "The winner is " ++ nameOf winner