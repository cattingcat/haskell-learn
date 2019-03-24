module Sub.MyWriter where 

import Prelude()
import Sub.KleisliCategory
import Data.Monoid

data MyWriter msg b = MyWriter (b, msg)

runWriter :: MyWriter msg b -> (b, msg)
runWriter (MyWriter a) = a


instance Monoid msg => Kleisli (MyWriter msg) where
    idK a = MyWriter (a, mempty)
    (*>) fa fb = \a -> MyWriter (foo a) where 
        foo a = let 
            (b, msg) = runWriter (fa a)
            (b', msg') = runWriter (fb b)
            in (b', mappend msg msg') 