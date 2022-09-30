module MJDRandom where

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Functor.Identity
import System.Random

newtype RandT g m a = RandT (StateT g m a)

unRandT :: RandT g m a -> StateT g m a
unRandT (RandT x) = x

-- runStateT  :: StateT s m a -> s -> m (a, s)
-- evalStateT :: StateT s m a -> s -> m a
runRandT :: RandT g m a -> g -> m (a, g)
runRandT (RandT st) = runStateT st

evalRandT :: Monad m => RandT g m a -> g -> m a
evalRandT (RandT st) = evalStateT st

-- Alex points out that all this can be automatically derived
instance (RandomGen g, Functor m) => Functor (RandT g m) where
    fmap f (RandT x) = RandT $ fmap f x

instance (RandomGen g, Monad m) => Applicative (RandT g m) where
    pure x = RandT $ pure x
    (RandT fm) <*> (RandT fx) = RandT (fm <*> fx)

instance (RandomGen g, Monad m) => Monad (RandT g m) where
    return = pure
    (RandT x) >>= f = RandT $ (x >>= (unRandT . f))

instance (RandomGen g, MonadIO m) => MonadIO (RandT g m) where
    liftIO x = RandT $ liftIO x

getUniform :: (Monad m, RandomGen g, Uniform r) => RandT g m r
getUniform = RandT $ do
    g <- get
    let (r, g') = uniform g
    put g'
    return r

getBool :: (Monad m, RandomGen g) => RandT g m Bool
getBool = getUniform
