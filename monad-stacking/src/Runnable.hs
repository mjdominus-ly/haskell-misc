module Runnable where

{-
class Runnable a b where
    run :: a -> b
    initState :: a

data Concrete1 = Conc1
    { stm :: Concrete
    , initState :: Integer
    }
data Concrete2 = Conc2
    { stm1 :: Concrete
    , stm2 :: Concrete
    , initState2 :: Integer
    }

instance Show Concrete where show _ = "<<stm>>"
instance Show Concrete1 where
    show c =
        let Identity (s', a) = run (stm c) (initState c)
         in (show i) ++ " -> " ++ (show s') ++ ": " ++ show a
      where
        i = initState c

instance Show Concrete2 where
    show c =
        let Identity (s1, a1) = run (stm1 c) (initState c)
            Identity (s2, a2) = run (stm2 c) (initState c)
         in (show i) ++ " -> " ++ (show s1) ++ " / " ++ (show s2) ++ ": " ++ show a2
      where
        i = initState c

instance Arbitrary Concrete1 where
    arbitrary = do
        c <- arbitrary
        i <- arbitrary
        return $ Conc1 c i

instance Arbitrary Concrete2 where
    arbitrary = do
        c1 <- arbitrary
        c2 <- arbitrary
        i <- arbitrary
        return $ Conc1 c1 c2 i

runConcrete1 :: Concrete1 -> Identity (Integer, Integer)
runConcrete1 c = run (stm c) (initState c) -}