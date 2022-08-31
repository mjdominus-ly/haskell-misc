module DirList where

import Control.Monad.IO.Class
import MJDState
import System.Directory
import System.IO

newtype FileInfo = FI (FilePath, Integer) deriving (Eq, Show)
data FIS = FIS {todo :: [FilePath], result :: [FileInfo]} deriving (Show)

inDir d fp = d ++ "/" ++ fp
qualify d = map (inDir d)

getTodo :: Monad m => StateT FIS m [FilePath]
getTodo = fmap todo get

trimTodo :: Monad m => StateT FIS m ()
trimTodo = do
    FIS (f : fs) res <- get
    put $ FIS fs res

tellResult :: Monad m => FileInfo -> StateT FIS m ()
tellResult r = do
    FIS todo result <- get
    put $ FIS todo (r : result)

ifEmpty :: Monad m => m [x] -> m a -> m a -> m a
ifEmpty mls mt me = do
    ls <- mls
    case ls of
        [] -> mt
        _ -> me

step' :: StateT FIS IO ()
step' = do
    ifEmpty
        getTodo
        (return ())
        ( do
            f : fs <- getTodo
            exists <- liftIO $ doesFileExist f
            if exists
                then (liftIO $ fileInfo f) >>= tellResult
                else return ()
            trimTodo
            step'
        )

main5 dir = do
    files <- listDirectory dir
    let paths = qualify dir files
    (final_state, _) <- run step' (initState paths)
    return final_state

step :: StateT FIS IO ()
step = do
    FIS todo result <- get
    case todo of
        [] -> return ()
        (f : fs) -> do
            exists <- liftIO $ doesFileExist f
            res <-
                if exists
                    then do
                        info <- liftIO $ fileInfo f
                        pure (info : result)
                    else pure result
            put $ FIS{todo = fs, result = res}
            step

initState :: [FilePath] -> FIS
initState files = FIS{todo = files, result = []}

main3 dir = do
    files <- listDirectory dir
    let paths = qualify dir files
    (final_state, _) <- run step (initState paths)
    return final_state

step2 :: StateT [FilePath] IO [FileInfo]
step2 = do
    todo <- get
    case todo of
        [] -> return []
        (f : fs) -> do
            put fs
            is <- step2
            exists <- liftIO $ doesFileExist f
            if exists
                then do
                    i <- liftIO $ fileInfo f
                    return (i : is)
                else return is

main4 dir = do
    files <- listDirectory dir
    let paths = map (inDir dir) files
    (_, v) <- run step2 paths
    return v

addFileInfo :: FilePath -> StateT [FileInfo] IO ()
addFileInfo f = do
    exists <- liftIO $ doesFileExist f
    if exists
        then do
            v <- liftIO $ fileInfo f
            update (v :)
        else return ()

fileInfo :: FilePath -> IO FileInfo
fileInfo fp = do
    size <- fileSize fp
    return $ FI (fp, size)

fileSize :: FilePath -> IO (Integer)
fileSize fp = withFile fp ReadMode hFileSize

mainn :: FilePath -> IO [FileInfo]
mainn dir = do
    files <- listDirectory dir
    let paths = map (inDir dir) files
    (final_state, _) <- run (mapM addFileInfo paths) []
    return final_state