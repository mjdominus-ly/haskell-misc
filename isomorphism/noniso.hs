{-# LANGUAGE RankNTypes #-}

import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Functor.Identity

type First e s a = ExceptT e (StateT s Identity) a
type Second e s a = StateT s (ExceptT e Identity) a

-- bind :: m a -> (a -> m b) -> m b

handle :: Either a b -> (a -> Either q b) -> Either q b
handle v handler = case v of Left exc -> handler exc
                             Right ok -> Right ok


divide :: (Monad m, Eq a, Fractional a) => a -> a -> ExceptT String m a
divide a b = 
    case b of 0 -> throwE "Division by zero"
              otherwise -> return $ a / b

testComp1 ::  (Eq a, Fractional a) => forall s . a -> First String s a
testComp1 d = divide 13 d


convert :: Monad m => ExceptT String m a -> StateT s (ExceptT String m) a
convert exceptt = StateT $ \s -> ExceptT $ do
    exc <- runExceptT exceptt
    case exc of
        Left errMsg -> return $ Left errMsg
        Right ok -> return $ Right (ok, s)

-- rewrite with handle
convert' :: Monad m => ExceptT String m a -> StateT s (ExceptT String m) a
convert' exceptt = StateT $ \s -> ExceptT $ do
    exc <- runExceptT exceptt
    return $ fmap (\ok -> (ok, s)) $ handle exc Left

-- rewrite with ???
convert'' :: Monad m => ExceptT String m a -> StateT s (ExceptT String m) a
convert'' exceptt = StateT $ \s -> ExceptT $ (fmap . fmap) (\ok -> (ok, s)) (runExceptT exceptt)

{-
testComp2 :: (Eq a, Fractional a) => forall s  . Second String s a
testComp2 = do
    d <- get
    -- _ $ divide 13.0 d
    _
-}

main :: IO ()
main = do
    print $ runStateT (runExceptT (testComp1 0)) 17
    -- print $ runExceptT (runStateT testComp2 17)
    -- print $ runStateT testComp2 0
    return ()