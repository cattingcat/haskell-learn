module Sub.KleisliCategory where

import Prelude(Int(..), Num(..), ($), const, foldr)

class Category cat where
    id :: cat a a
    (.) :: cat b c -> cat a b -> cat a c

instance Category (->) where
    id = \x -> x
    f . g = \x -> f (g x)

         
class Kleisli m where
    idK :: a -> m a 
    (*>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
        
(*$) :: (Kleisli m) => (a -> m b) -> m a -> m b
(*$) f ma = ((const ma) *> f)()

(+$) :: (Kleisli m) => (a -> b) -> m a -> m b
(+$) f ma = (idK . f) *$ ma

lift1 :: (Kleisli m) => (a -> b) -> m a -> m b
lift1 = (+$) 

($$) :: (Kleisli m) => m (a -> b) -> m a -> m b
($$) mf ma = (+$ ma) *$ mf

lift2 :: (Kleisli m) => (a -> b -> c) -> m a -> m b -> m c
lift2 f ma mb = lift1 f ma $$ mb

sequence' :: (Kleisli m) => [m a] -> m [a]
sequence' = foldr (lift2 (:)) (idK [])
-- sequence' [] = idK []
-- sequence' (h:t) = (const h *> (\hh -> (const (sequence' t) *> \l -> idK (hh:l))()))()
