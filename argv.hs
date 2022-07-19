module Argv where

import System.Environment (getArgs)
import System.IO ()

input :: IO String
input = do
    argv <- getArgs
    case argv of
        [] -> getContents -- stdin is implied here
        _ -> concat <$> mapM readFile argv