
module MJDState where

import Control.Monad.Identity
import System.IO.Unsafe
import Control.Applicative

newtype StateT s m a = StateT (s -> m (s, a))

run :: StateT s m a -> s -> m (s, a)
run (StateT z) = z

instance Functor f => Functor (StateT s f) where
         fmap f (StateT z) = StateT $ \s -> fmap  (toSnd f) (z s)
           where toSnd f (c1, c2)  = (c1, f c2)

instance Applicative f => Applicative (StateT s f) where
         pure a = StateT $ \s -> pure (s, a)
         (StateT zf) <*> (StateT za) = StateT $
                 \s -> let ap1   = zf s
                           ap2   = za s
                       in liftA2 comb ap1 ap2 where
                         comb = \(a1, b1) (a2, b2)  -> (a1, b1 b2)  -- a1? a2?

-- StateT s m a -> (a -> StateT s m b) -> StateT s m b   
instance Monad m => Monad (StateT s m) where
    return    = pure
    (StateT za) >>= f    =
       StateT $ \s ->
           do
             (s', a)  <- za s
             run (f a) s'

tr :: String -> a -> a
tr s v = (unsafePerformIO (print s) `seq` v)
    
get :: (Show s, Monad m) => StateT s m s
get = StateT $ \s -> tr ("got: " ++ show s) (return (s, s))
       
put :: (Show s, Monad m) => s -> StateT s m ()
put s = StateT $ \s' -> tr ("put: " ++ show s ++ " (was " ++ show s' ++")") (return (s, ()))

init :: Monad m => s -> StateT s m ()
init s = StateT $ \_ -> return (s, ())

type State s a = StateT s Identity a
