{-# LANGUAGE GADTs #-}

module Sub.GadtExample where

import Control.Applicative

data MyTmp a where
    Qwe :: Monad f => f a -> MyTmp (f a)
    Asd :: MyTmp a

t1 :: MyTmp Int
t1 = Asd

t2 :: MyTmp [Int]
t2 = Qwe [5]