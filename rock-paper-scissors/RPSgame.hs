{-# LANGUAGE RankNTypes #-}

module RPSgame (
    makeHumanPlayer,
    makeComputerPlayer,
    makeGame,
    nameOf,
    Player,
) where

import Control.Monad.IO.Class
import Data.List
import MJDRandom
import RPS
import System.Environment
import System.IO
import System.Random

data Player = Player (forall m. MonadIO m => Player -> RandT m RPS) String
makeHumanPlayer :: String -> Player
makeHumanPlayer = Player promptMove

makeComputerPlayer :: Int -> Player
makeComputerPlayer n = Player randomMove name
  where
    name = "COMPUTER-X0" ++ show n

randomMove :: MonadIO m => Player -> RandT m RPS
randomMove p = do
    m <- getUniform
    cheat <- liftIO $ lookupEnv "RPSCHEAT"
    case cheat of
        Just x
            | x /= "" && x /= "0" ->
                liftIO $ putStrLn $ nameOf p ++ " intends to throw: " ++ show m
        _ -> return ()
    return m

promptMove :: MonadIO m => Player -> m RPS
promptMove p = liftIO $ do
    putStr $ "Your move, " ++ nameOf p ++ "? "
    hFlush stdout
    play <- fromString <$> getLine
    case play of
        Just rps -> return rps
        Nothing -> do
            putStrLn $ "Can't understand that, enter 'r', 'p', or 's'"
            promptMove p

reportRound :: RPS -> RPS -> String
reportRound m y = do
    case compareRPS m y of
        GT -> report m y
        LT -> report y m
        EQ -> "It's a tie."
  where
    -- "Scissors cuts paper"
    report w l = intercalate " " [show w, winVerb w, show l]

nameOf :: Player -> String
nameOf (Player _ n) = n

genMove :: Player -> RandT IO RPS
genMove p@(Player m _) = m p

-- returns the winner
makeGame :: Player -> Player -> RandT IO Player
makeGame p1 p2 = game
  where
    game = do
        t1 <- genMove p1
        t2 <- genMove p2

        liftIO $ do
            putStrLn $ nameOf p1 ++ " threw: " ++ show t1
            putStrLn $ nameOf p2 ++ " threw: " ++ show t2
            putStrLn $ reportRound t1 t2
        case compareRPS t1 t2 of
            GT -> return p1
            LT -> return p2
            EQ -> (liftIO $ putStrLn "Another round!") >> game