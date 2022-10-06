{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.IO.Class
import Data.Int
import System.Environment
import System.Random

import MJDRandom
import RPSgame

main :: IO ()
main = do
    (p1, p2) <- makePlayers
    rng <- initStdGen
    winner <- evalRandT (makeGame p1 p2) rng
    putStrLn $ "The winner is " ++ nameOf winner

makePlayers :: IO (Player, Player)
makePlayers = do
    args <- getArgs
    case args of
        [] -> return (c 1, c 2)
        [n] -> return (c 1, makeHumanPlayer n)
        [n1, n2] -> return $ (makeHumanPlayer n1, makeHumanPlayer n2)
        _ -> usage
  where
    c = makeComputerPlayer

usage = error "Usage: RPS [player-1-name [player-2-name]]"