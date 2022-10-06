{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MJDRandom (
    evalRandT,
    RandT,
    getUniform,
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import System.Random

newtype RandT g m a = RandT (StateT g m a)
    deriving (Functor, Applicative, Monad, MonadIO)

unRandT :: RandT g m a -> StateT g m a
unRandT (RandT x) = x

runRandT :: RandT g m a -> g -> m (a, g)
runRandT (RandT st) = runStateT st

evalRandT :: Monad m => RandT g m a -> g -> m a
evalRandT (RandT st) = evalStateT st

getUniform :: (Monad m, RandomGen g, Uniform r) => RandT g m r
getUniform = RandT $ do
    g <- get
    let (r, g') = uniform g
    put g'
    return r

getBool :: (Monad m, RandomGen g) => RandT g m Bool
getBool = getUniform
