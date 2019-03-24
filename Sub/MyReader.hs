module Sub.MyReader where

import Prelude()
import Sub.KleisliCategory

data Reader env b = Reader(env -> b)

runReader :: Reader env b -> env -> b
runReader (Reader f) = f

instance Kleisli (Reader env) where
    idK a = Reader(\env -> a)
    (*>) fa fb = \a -> Reader (foo a) where
        foo a env = let b = runReader (fa a) env in runReader (fb b) env