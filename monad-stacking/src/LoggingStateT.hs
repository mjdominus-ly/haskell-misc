{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module LoggingStateT where

import Control.Monad.Identity
import Control.Monad.Trans.Writer
import Prelude hiding (log)

newtype StateT s m a = StateT (s -> m (s, a))
instance Show (StateT s m a) where
    show _ = "<<stm>>"

instance Functor fc => Functor (StateT s fc) where
    -- f :: (a -> b) -> (StateT s fc a -> StateT s fc b)
    fmap f x = StateT $ \s -> fmap (at_snd f) (run x s)
      where
        at_snd f (p, q) = (p, f q)

instance Monad m => Applicative (StateT s m) where
    pure x = StateT $ \s -> pure (s, x)
    fm <*> vm = StateT $
        \s0 -> do
            (s1, f) <- run fm s0
            (s2, v) <- run vm s1
            return (s2, f v)

instance Monad m => Monad (StateT s m) where
    return = pure
    (StateT za) >>= f =
        StateT $ \s ->
            do
                (s', a) <- za s
                run (f a) s'

newtype LogWriterMonad m a = LogWriterMonad (WriterT [String] m a)
unwrap (LogWriterMonad x) = x

class Monad m => LogWriterClass m where
    log :: String -> m ()

instance Functor m => Functor (LogWriterMonad m) where
    fmap f (LogWriterMonad x) = LogWriterMonad (fmap f x)

instance Applicative m => Applicative (LogWriterMonad m) where
    pure = LogWriterMonad . pure
    (LogWriterMonad f) <*> (LogWriterMonad x) =
        LogWriterMonad (f <*> x)

instance Monad m => Monad (LogWriterMonad m) where
    return = LogWriterMonad . return
    (LogWriterMonad w) >>= f = LogWriterMonad $ w >>= (unwrap . f)

instance Monad m => LogWriterClass (LogWriterMonad m) where
    log s = LogWriterMonad $ (tell [s])

instance LogWriterClass Identity where
    log _ = return ()

instance LogWriterClass IO where
    log = print

instance Monad m => LogWriterClass (WriterT [String] m) where
    log s = tell [s]

get :: (Show s, LogWriterClass m) => StateT s m s
get =
    StateT
        ( \s -> do
            log $ "got " ++ show s
            return (s, s)
        )

put :: (Show s, LogWriterClass m) => s -> StateT s m ()
put s' =
    StateT
        ( \s -> do
            log $ "put " ++ show s ++ "->" ++ show s'
            return (s', ())
        )

run :: StateT s m a -> s -> m (s, a)
run (StateT z) s = z s

test :: StateT Integer (WriterT [String] Identity) ()
test = do
    a <- get
    put $ 2 * a

mainn = do
    run test 7
    return ()
