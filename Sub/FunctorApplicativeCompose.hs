module Sub.FunctorApplicativeCompose where

import Control.Applicative

newtype O f g a = O { unO :: f (g a) }

instance (Functor f, Functor g) => Functor (O f g) where
    fmap ff O{unO=t} = let 
        gm = fmap ff
        fm = fmap gm
        in O{unO = fm t}


instance (Applicative f, Applicative g) => Applicative (O f g) where
    pure a = O { unO = pure (pure a) }
    (<*>) O{unO=fgf} O{unO=fga} = let   -- fgf  :: f g (a -> b) -> f g a -> f g b
                                        -- fga  :: f g a
        ff = descentF fgf
        gf = (<*>) ff
        in O{unO = gf fga}

descentF :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a -> g b)
descentF fgf = (pure (\gf -> (<*>) gf)) <*> fgf


fmap2 :: (Applicative f, Applicative g) => (a -> b) -> f (g a) -> f (g b)
fmap2 f fga = unO $ fmap f O{ unO = fga }

liftA22 :: (Applicative f, Applicative g) => (a -> b -> c) -> f (g a) -> f (g b) -> f (g c)
liftA22 f fga fgb = unO $ liftA2 f O{unO = fga} O{unO = fgb}


test :: O Maybe [] Int
test = fmap (\x -> x + 100) O{ unO = (Just [5]) }

run1 = unO test
run2 = fmap2 (*2) [[1,2,3], [3,4,5]]
run3 = fmap2 (*2) [Just 3, Just 4, Just 5, Nothing]
run4 = liftA22 (+)  (Just (Right 52)) (Just (Right 5))