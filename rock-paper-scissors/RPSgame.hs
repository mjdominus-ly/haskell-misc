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

data Player = Player (Player -> RandT StdGen IO RPS) String
makeHumanPlayer :: String -> Player
makeHumanPlayer = Player promptMove

makeComputerPlayer :: Int -> Player
makeComputerPlayer n = Player randomMove $ "COMPUTER-" ++ show n

randomMove :: Player -> RandT StdGen IO RPS
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
    if m == y
        then "It's a tie."
        else
            let (w, l) = sortP m y
             in show w ++ " " ++ winVerb w ++ " " ++ show l
  where
    sortP :: Ord a => a -> a -> (a, a)
    sortP a b = if a > b then (a, b) else (b, a)

nameOf :: Player -> String
nameOf (Player _ n) = n

moveGen :: Player -> RandT StdGen IO RPS
moveGen p@(Player m _) = m p

-- returns the winner
makeGame :: Player -> Player -> RandT StdGen IO Player
makeGame p1 p2 = game
  where
    game = do
        a <- moveGen p1
        b <- moveGen p2

        liftIO $ do
            putStrLn $ nameOf p1 ++ " threw: " ++ show a
            putStrLn $ nameOf p2 ++ " threw: " ++ show b
            putStrLn $ reportRound a b
        case compare a b of
            GT -> return p1
            LT -> return p2
            EQ -> (liftIO $ putStrLn "Another round!") >> game