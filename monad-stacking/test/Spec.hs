{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.Identity
import Control.Monad.Identity (Identity)
import MJDState

-- import Test.QuickCheck hiding (sample)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (sample)

main :: IO ()
main = defaultMain $ testGroup "main" [identityLaw, homomorphismLaw, interchangeLaw, compositionLaw]

-- StateT (Integer -> Identity (Integer, a)
newtype Concrete a = Conc (Fun Integer (Identity (Integer, a)))
    deriving (Show, Arbitrary)

up :: Concrete a -> StateT Integer Identity a
up (Conc f_) = StateT (applyFun f_)
runC :: Concrete a -> Integer -> Identity (Integer, a)
runC = run . up

--  1. pure id <*> v  =  v
identityLaw =
    testProperty "id law" prop
  where
    prop :: Concrete Integer -> Integer -> Property
    prop v i = counterexample msg $ run lhs i == run rhs i
      where
        lhs = pure id <*> up v
        rhs = up v
        msg = show $ ("ID", "init state: ", i, "; v=", v)

--  2. pure f <*> pure x = pure (f x)
homomorphismLaw =
    testProperty "hom law" prop
  where
    prop :: Fun Integer Integer -> Integer -> Integer -> Property
    prop f_ x i = counterexample msg $ run lhs i == run rhs i
      where
        lhs = pure f <*> pure x
        rhs = pure (f x) :: StateT Integer Identity Integer
        f = applyFun f_
        msg = show $ ("HOM f=", f_, "; x=", x, "state=" ++ show i)

--  3. u <*> pure y = pure ($ y) <*> u
interchangeLaw =
    testProperty "xch law" prop
  where
    prop :: Concrete (Fun Integer Integer) -> Integer -> Integer -> Property
    prop u' y i = counterexample msg $ run lhs i == run rhs i
      where
        lhs = u <*> pure y
        rhs = pure ($ y) <*> u
        u = fmap applyFun (up u')
        msg = show ("XCH u=", u', "; y=", y, "; init state=", i)

--  4. u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
compositionLaw =
    testProperty "comp law" prop
  where
    prop ::
        Concrete (Fun Integer Integer) ->
        Concrete (Fun Integer Integer) ->
        Concrete Integer ->
        Integer ->
        Property
    prop u' v' w' i = counterexample msg $ run lhs i == run rhs i
      where
        lhs = u <*> (v <*> w)
        rhs = pure (.) <*> u <*> v <*> w
        (u, v) = (fmap applyFun (up u'), fmap applyFun (up v'))
        w = up w'
        msg = (show $ ("CMP u=", u', "; v=", v', "; w=", w'))

-- laws
--  1. pure id <*> v  =  v
--  2. pure f <*> pure x = pure (f x)
--  3. u <*> pure y = pure ($ y) <*> u
--  4. u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
