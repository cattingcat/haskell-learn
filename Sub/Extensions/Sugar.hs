{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Sub.Extensions.Sugar where

test = [1..10]
test2 = [1, 5 .. 100]

data Tmp = A1 | B2 | C3 | D4 | E5 | F6 deriving (Show, Enum, Eq)

-- Enum typeclass required
test3 = [A1, C3 .. F6]
test4 = [x | x <- [-10 .. 10], even x, x >= 0]

-- LANGUAGE MultiParamTypeClasses  -- class SomeClass a b where
-- LANGUAGE GADTs.  -- data SomeDate where A :: Int -> a

class AdditiveGroup v where
    zeroV :: v
    (^+^) :: v -> v -> v
    negateV :: v -> v

class AdditiveGroup v => VectorSpace v where
    type Scalar v :: *
    (*^) :: Scalar v -> v -> v


data Point2d a = Point2d a a deriving (Show, Eq)

instance AdditiveGroup Int where
    zeroV = 0
    (^+^) = (+)
    negateV = negate

instance AdditiveGroup a => AdditiveGroup (Point2d a) where
    zeroV = Point2d zeroV zeroV
    (^+^) (Point2d x1 y1) (Point2d x2 y2) = Point2d (x1 ^+^ x2) (y1 ^+^ y2)
    negateV (Point2d x y) = Point2d (negateV x) (negateV y)

instance VectorSpace (Point2d Int) where
    type Scalar (Point2d Int) = Int
    (*^) n (Point2d x y) = Point2d (n * x) (n * y)


testVs = 55 *^ ((Point2d 4 5) :: Point2d Int)