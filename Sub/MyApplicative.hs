module Sub.MyApplicative where

import Prelude (($))

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

liftA2 :: (Applicative f) => f (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <*> a <*> b

seqA :: (Applicative f) => [f a] -> f [a]
seqA ([]) = pure []
seqA (h:t) =  concatf h (seqA t) where 
    concatf :: (Applicative f) => f a -> f [a] -> f [a]
    concatf = liftA2 (pure (:))
    