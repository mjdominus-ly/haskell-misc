
module MJDState where

import Control.Monad.Identity
import System.IO.Unsafe

newtype StateT s m a = StateT (s -> (s, m a))

run :: StateT s m a -> s -> (s, m a)
run (StateT z) = z

instance Functor m => Functor (StateT s m) where
         fmap f (StateT z) = StateT $ \s -> let (ns, a) = z s in (ns, fmap f a)

instance Applicative m => Applicative (StateT s m) where
         pure a = StateT $ \s -> (s, pure a)
         (StateT zf) <*> (StateT za) = StateT $
                 \s -> let (s', f) = zf s
                           (s'', a) = za s'
                       in (s'', f <*> a)

-- worried about the _ here
-- StateT s m a -> (a -> StateT s m b) -> StateT s m b   
instance Monad m => Monad (StateT s m) where
    return    = pure
    (StateT za) >>= f    =
       StateT $ \s ->
           let (s', ma) = (za s)
               mb = ma >>= (\a -> snd $ run (f a) s')
           in (s', mb)
    
    -- (StateT za) >>= f =
    --     StateT $ \s -> 
    --       let (s', ma)  = za s
    --           x = (ma >>= (\a -> run (f a) s'))
    --           xx = run (f a) s' :: _
    --       in xx

tr :: String -> a -> a
tr s v = (unsafePerformIO (print s) `seq` v)
    
get :: (Show s, Monad m) => StateT s m s
get = StateT $ \s -> tr ("got: " ++ show s) (s, return s)
       
put :: (Show s, Monad m) => s -> StateT s m ()
put s = StateT $ \s' -> tr ("put: " ++ show s ++ " (was " ++ show s' ++")") (s, return ())

init :: Monad m => s -> StateT s m ()
init s = StateT $ \_ -> (s, return ())

type State s a = StateT s Identity a

