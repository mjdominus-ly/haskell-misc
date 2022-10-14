
import Control.Monad.Trans.Reader
import Data.Functor.Identity

-- Can you find the functions which witness the two directions of the isomorphism
-- between 


type First r a = Reader r (IO a)
type Second r a = ReaderT r IO a

right :: First r a -> Second r a
left :: Second r a -> First r a

right (ReaderT x) = 
    ReaderT (runIdentity . x)

left (ReaderT x) = 
    ReaderT (Identity . x)