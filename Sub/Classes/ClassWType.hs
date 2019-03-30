{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Sub.Classes.ClassWType where

class SomeClass a b where
    type SomeType a b
    foo :: a -> b -> SomeType a b


instance SomeClass String Int where
    type SomeType String Int = Float
    foo s i = 55.55

instance Num n => SomeClass n n where
    type SomeType n n = n
    foo a b = a + b

-- TODO: How to have two instance with same signature
-- instance Monoid m => SomeClass m m where
--     type SomeType m m = m
--     foo a b = mcombine a b

-- test: foo "qwe" (2 :: Int)



class AdditiveGroup v where
    zeroV :: v
    (^+^) :: v -> v -> v
    negateV :: v -> v

class AdditiveGroup v => VectorSpace v where
    type Scalar v :: *
    (*^) :: Scalar v -> v -> v

data Point2d = Point2d Int Int
 