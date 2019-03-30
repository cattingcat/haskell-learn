{-# LANGUAGE GADTs #-}
{-# Language FlexibleContexts, UndecidableInstances, TypeSynonymInstances, FlexibleInstances #-}

module Sub.CatTypes where

import Prelude hiding (succ)
import Sub.FixedPointCombinator

data Tmp = Som | Non

sumInt :: Int -> Int 
sumInt 0 = 0
sumInt a = a + sumInt(a - 1)

prodInt :: Int -> Int
prodInt = fix $ \f n -> 
    case n of
        0 -> 1
        k -> k * f(k - 1)

newtype Fix f = Fix { unFix :: f (Fix f) }

data N a = Zero | Succ a deriving (Show, Eq)

instance Functor N where 
    fmap f Zero = Zero
    fmap f (Succ a) = Succ (f a)

type Nat = Fix N

zero :: Nat 
zero = Fix Zero

succ :: Nat -> Nat
-- succ n = Fix (Succ n)
succ = Fix . Succ


instance Show (f (Fix f)) => Show (Fix f) where
    show x = "(" ++ show (unFix x) ++ ")"

instance Eq (f (Fix f)) => Eq (Fix f) where
    a == b = unFix a == unFix b

data L a b = Nil | Cons a b deriving (Show, Eq)

instance Functor (L a) where 
    fmap f Nil = Nil
    fmap f (Cons a b) = Cons a (f b)

type List a = Fix (L a)

nil :: List a
nil = Fix Nil

cons :: a -> List a -> List a
cons a = Fix . Cons a

infixr 5 `cons`



fold :: Functor f => (f b -> b) -> Fix f -> b
fold f fx = let 
    fFix = unFix fx         -- f (Fix f)
    fb = fmap (fold f) fFix -- f b
    in f fb                 -- b
-- fold f = f . fmap (fold f) . unFix


unfold :: Functor f => (a -> f a) -> a -> Fix f
unfold f a = let 
    fa = f a
    fffffa = fmap (unfold f) fa
    in Fix fffffa
-- unfold f = fix . fmap (unfold f) . f





instance Num Nat where
    (+) a = fold $ \x -> case x of
        Zero -> a
        Succ x -> succ x
    (*) a = fold $ \x -> case x of
        Zero -> zero
        Succ x -> a + x
    fromInteger = unfold $ \n -> case n of
        0 -> Zero
        n -> Succ (n-1)
    negate = undefined
    abs = undefined
    signum = undefined




testList = 1 `cons` 2 `cons` nil

test = fold foo testList where
    foo Nil = 0
    foo (Cons a b) = a + b

-- data K a where
--     KZero :: K a
--     KSucc :: a -> K a
-- type KNat = Fix K

-- kzero :: KNat
-- kzero =  Fix KZero

-- ksucc :: KNat -> KNat
-- ksucc n = Fix (KSucc n)