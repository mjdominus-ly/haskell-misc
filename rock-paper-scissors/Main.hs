{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.IO.Class
import Data.Int
import MJDRandom
import RPS
import RPSgame
import System.Random

main :: IO ()
main = do
    g <- initStdGen
    res <- evalRandT game g
    putStrLn $ "Result: " ++ show res
