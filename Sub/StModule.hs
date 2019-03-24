module Sub.StModule where

import Prelude(Int(..), Num(..), ($), const, foldr)
import Sub.KleisliCategory

data St a b = St (a -> (b, St a b))

foo :: St Int Int 
foo = St (\x -> (x + 1, foo))

ap :: St a b -> [a] -> [b]
ap (St f) [] = []
ap (St f) (h : t) = let (r, st') = f h in r : (ap st' t)


instance Category St where 
    id = St f where f = \x -> (x, St f)
    (.) a b = St f where 
        St fa = a
        St fb = b
        f = \x -> let 
            (x2, b') = fb x
            (x1, a') = fa x2
            in (x1, (a' . b'))

integral :: Num a => St a a
integral = St (\x -> (x, loop x)) where 
    loop z = St (\x -> let next = z + x in (next, loop next))
   