module DirList where

import Control.Monad.IO.Class
import MJDState
import System.Directory
import System.IO

newtype FileInfo = FI (FilePath, Integer)

{- data FileIterationState b = FIS {todo :: [FilePath], result :: [b]}

nextUp tSTM = do
    t <- get
    case t of
        FIS{todo = [], result = r} -> return $ (t, ())
        FIS{todo = (f : fs), result = r} -> do
            info <- fileInfo f
            (FIS{todo = fs, result = fileInfo f : r}, ()) -}

accumulate :: (Show s, Monad m) => s -> StateT [s] m ()
accumulate x = (x :)

pass :: Monad m => m ()
pass = return ()

addFileInfo :: FilePath -> StateT [(FilePath, Integer)] IO ()
addFileInfo f = do
    exists <- liftIO $ doesFileExist f
    if exists
        then do
            v <- liftIO $ fileInfo f
            accumulate v
        else pass

fileInfo :: FilePath -> IO (FilePath, Integer)
fileInfo fp = do
    size <- fileSize fp
    return (fp, size)

fileSize :: FilePath -> IO (Integer)
fileSize fp = withFile fp ReadMode hFileSize

inDir d fp = d ++ "/" ++ fp

mainn :: FilePath -> IO [(FilePath, Integer)]
mainn dir = do
    files <- listDirectory dir
    let paths = map (inDir dir) files
    (final_state, _) <- run (mapM addFileInfo paths) []
    return final_state
