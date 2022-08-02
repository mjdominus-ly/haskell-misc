{-# LANGUAGE RankNTypes #-}

module SYB where

import Unsafe.Coerce

-- --------------

data TypeRep = TR String [TypeRep]
  deriving (Eq, Show)

class Typeable a where
  typeOf :: a -> TypeRep

instance Typeable a => Typeable [a] where
  typeOf x = TR "List" [typeOf (head x)]

instance Typeable Char where
  typeOf x = TR "Char" []

instance Typeable Int where
  typeOf x = TR "Int" []

{- instance (Typeable a, Typeable b) => Typeable (a -> b) where
  typeOf f = TR "Function" [typeOf (argOf f), typeOf (resOf f)]
   where
    resOf :: (a -> b) -> b
    resOf _ = undefined
    argOf :: (a -> b) -> a
    argOf _ = undefined -}

instance (Typeable a, Typeable b) => Typeable (a -> b) where
  typeOf f = TR "Function" [typeOf arg, typeOf res]
   where
    res = f arg
    arg = undefined

instance Typeable a => Typeable (Maybe a) where
  typeOf x = TR "Maybe" [typeOf (unm x)]
   where
    unm :: Maybe a -> a
    unm = undefined

-- ------------

cast :: (Typeable a, Typeable b) => a -> Maybe b
cast a = r
 where
  r = if typeOf a == typeOf (unm r) then Just (unsafeCoerce a) else Nothing
  unm :: Maybe a -> a
  unm = undefined

-- --------------

--
-- this works, for example
--   ext toUpper 'c'         yields  'C'
--   ext toUpper (17 :: Int) yields  17
--
-- But `ext tail` doesn't work; why not?
--    I bet it's because it needs Typeable [a] but without
--      Typeable a it can't prove that.
--    Indeed, `ext (tail :: String -> String)` works fine.

ext :: (Typeable b, Typeable a) => (a -> a) -> (b -> b)
ext f = res
 where
  res = case cast f of
    Just f' -> f'
    Nothing -> id