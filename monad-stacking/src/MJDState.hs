{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MJDState where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Identity
import System.IO.Unsafe

newtype StateT s m a = StateT (s -> m (s, a))

instance Show (StateT s m a) where show _ = "<<stm>>"

run :: StateT s m a -> s -> m (s, a)
run (StateT z) = z

instance Functor fc => Functor (StateT s fc) where
  fmap :: forall a b. (a -> b) -> StateT s fc a -> StateT s fc b
  fmap f stm = StateT $ fz
   where
    toSnd :: (p -> q) -> (c, p) -> (c, q)
    toSnd f (c1, c2) = (c1, f c2)
    lifted_f :: forall c. (c, a) -> (c, b)
    lifted_f = toSnd f
    fz :: s -> fc (s, b)
    fz state = fmap lifted_f (run stm state)

-- laws
--  1. pure id <*> v  =  v
--  2. pure f <*> pure x = pure (f x)
--  3. u <*> pure y = pure ($ y) <*> u
--  4. u <*> (v <*> w) = pure (.) <*> u <*> v <*> w

{- instance Applicative f => Applicative (StateT s f) where
  pure a = StateT $ \s -> pure (s, a)
  stf <*> stv = StateT $
    \s ->
      let apf = run stf s
          apv = run stv s
       in liftA2 comb apf apv
   where
    comb = \(s1, f) (s2, v) -> (s1, f v) -- s1? s2?

 -}

instance Monad f => Applicative (StateT s f) where
  pure a = StateT $ \s -> pure (s, a)
  fm <*> vm = StateT $
    \s0 -> do
      (s1, v) <- run vm s0
      (s2, f) <- run fm s1
      return (s2, f v)

--  m1 <*> m2 = m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2)))
--  m1 <*> m2 = m2 >>= (\x2 -> m1 >>= (\x1 -> return (x1 x2)))

-- StateT s m a -> (a -> StateT s m b) -> StateT s m b
-- is   a >>= b    supposed to be the same as a <*> pure b ??
instance Monad m => Monad (StateT s m) where
  return = pure
  (StateT za) >>= f =
    StateT $ \s ->
      do
        (s', a) <- za s
        run (f a) s'

instance MonadIO m => MonadIO (StateT s m) where
  -- liftIO :: IO a -> StateT s m a
  liftIO ioa = StateT $ \s ->
    do
      a <- liftIO ioa
      return (s, a)

tr :: String -> a -> a
tr s v = (unsafePerformIO (print s) `seq` v)

get :: (Show s, Monad m) => StateT s m s
get = StateT $ \s -> tr ("got: " ++ show s) (return (s, s))

put :: (Show s, Monad m) => s -> StateT s m ()
put s = StateT $ \s' -> tr ("put: " ++ show s ++ " (was " ++ show s' ++ ")") (return (s, ()))

init :: Monad m => s -> StateT s m ()
init s = StateT $ \_ -> return (s, ())

update :: (Show s, Monad m) => (s -> s) -> StateT s m ()
update f = do
  s <- get
  put $ f s

(===) :: (Eq (m (s, a)), Num s) => StateT s m a -> StateT s m a -> Bool
s1 === s2 =
  run s1 0 == run s2 0

set_to_7 = put 7 >> pure id :: StateT Integer Identity (a -> a)

inc :: x -> StateT Integer Identity x
inc f = (get >>= \s -> put (s + 1)) >> return f

--  3. u <*> pure y = pure ($ y) <*> u
law3 u y = (u <*> pure y) === (pure ($ y) <*> u)

--  4. u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
law4 :: (Eq c, Eq s, Num s) => StateT s Identity (b -> c) -> StateT s Identity (a -> b) -> StateT s Identity a -> Bool
law4 u v w =
  let lt = (u <*> (v <*> w))
      rt = (pure (.) <*> u <*> v <*> w)
      Identity (s1, f1) = run lt 0
      Identity (s2, f2) = run rt 0
   in s1 == s2 && f1 == f2

type State s a = StateT s Identity a
