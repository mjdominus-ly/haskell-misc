module RPSgame where

import Control.Monad.IO.Class
import MJDRandom
import RPS
import System.Random

data GameResult = IWin | YouWin | Tie
    deriving (Eq, Show, Enum)

myMove :: RandT StdGen IO RPS
myMove = do
    m <- getUniform
    liftIO $ putStrLn $ "I intend to throw: " ++ show m
    return m

yourMove :: IO RPS
yourMove = do
    putStr "Your move? "
    fromString <$> getLine

game :: RandT StdGen IO GameResult
game = do
    m <- myMove
    y <- liftIO yourMove
    liftIO $ putStrLn $ "I threw: " ++ show m
    case compare m y of
        GT -> return IWin
        LT -> return YouWin
        EQ -> do
            liftIO $ putStrLn "It's a tie. Another round!"
            game