module RPSgame where

import Control.Monad.IO.Class
import Data.List
import MJDRandom
import RPS
import System.IO
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
    hFlush stdout
    fromString <$> getLine

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

game :: RandT StdGen IO GameResult
game = do
    m <- myMove
    roundResult <- liftIO $ do
        y <- yourMove
        putStrLn $ "I threw: " ++ show m
        putStrLn $ reportRound m y
        return $ case compare m y of
            GT -> IWin
            LT -> YouWin
            EQ -> Tie
    case roundResult of
        Tie -> (liftIO $ putStrLn "Another round!") >> game
        x -> return x