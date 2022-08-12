module Main where

import MJDState

s :: State Int ()
s = MJDState.init 3

a :: State Int ()
a = do
  s <- get
  put $ s + s
  t <- get
  return ()

b :: State Int ()
b = do
  s <- get
  put 12
  t <- get
  put 19
  u <- get
  return ()

  
c :: State Int String
c = do
  s <- get
  put $ s + s
  return "Done"

-- b :: State Int ()
-- b = get >>= (\s -> put $ s + s) >>= get  
  

--  z <- get
--  put $ z + z

main :: IO ()
main = print $ run c 77
