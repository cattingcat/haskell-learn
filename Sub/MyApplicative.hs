module Sub.MyApplicative where

import Prelude (($), id, const, (.))

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b


(<$) :: Functor f => a -> f b -> f a
(<$) = fmap . const
--(<$) a fb = fmap (const a) fb

(*>) :: Applicative f => f a -> f b -> f b 
(*>) fa fb = id <$ fa <*> fb
--(*>) = liftA2 (pure $ \x y -> y)


liftA2 :: (Applicative f) => f (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <*> a <*> b

seqA :: (Applicative f) => [f a] -> f [a]
seqA ([]) = pure []
seqA (h:t) =  concatf h (seqA t) where 
    concatf :: (Applicative f) => f a -> f [a] -> f [a]
    concatf = liftA2 (pure (:))
    