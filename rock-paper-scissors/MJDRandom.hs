{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MJDRandom (
    evalRandT,
    RandT,
    getUniform,
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import System.Random

newtype RandT m a = RandT (StateT StdGen m a)
    deriving (Functor, Applicative, Monad, MonadIO)

unRandT :: RandT m a -> StateT StdGen m a
unRandT (RandT x) = x

runRandT :: RandT m a -> StdGen -> m (a, StdGen)
runRandT (RandT st) = runStateT st

evalRandT :: (Monad m) => RandT m a -> StdGen -> m a
evalRandT (RandT st) = evalStateT st

getUniform :: (Monad m, Uniform r) => RandT m r
getUniform = RandT $ do
    g <- get
    let (r, g') = uniform g
    put g'
    return r

getBool :: Monad m => RandT m Bool
getBool = getUniform
