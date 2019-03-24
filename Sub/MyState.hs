module Sub.MyState where

import Prelude(Num(..), Functor(..), Applicative(..), Monad(..), Int(..))
import Sub.KleisliCategory

data State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State f) = f

-- alternative declaration
-- newtype State s a = State { runState :: s -> (a, s) }

instance Kleisli (State s) where
    idK a = State (\s -> (a, s))
    (*>) f g = \a -> State (foo a) where 
        foo a s = let (b, fs) = runState (f a) s 
            in runState (g b) fs


instance Functor (State s) where 
    fmap f fa = State foo where
        foo s = let (a, s') = runState fa s in (f a, s')

instance Applicative (State s) where 
    pure a = State (\s -> (a, s))
    -- (<*>) :: f (a -> b) -> f a -> f b
    (<*>) sf sa = State foo where
        foo s = let (f, s') = runState sf s in runState (fmap f sa) s'


instance Monad (State s) where 
    return a = State (\s -> (a, s))
    (>>=) ma f = State foo where 
        foo s = let (a, as) = runState ma s in runState (f a) as

sumState :: Num a => State a a
sumState = State (\x -> (x + 1, x))

x2State :: State Int Int
x2State = State (\x -> (x * 2, x))

comboState = sumState >>= (\i -> x2State)

run = runState comboState