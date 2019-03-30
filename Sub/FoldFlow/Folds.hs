{-# LANGUAGE GADTs #-}

module Sub.FoldFlow.Folds where 


foldBool :: a -> a -> Bool -> a
foldBool t f True = t
foldBool t f False = f

not' :: Bool -> Bool
not' = foldBool False True

(&&&), (|||) :: Bool ->  Bool -> Bool
(&&&) = foldBool id (const False)
(|||) = foldBool (const True) id 
    

data Nat where
    Zero :: Nat
    Succ :: Nat -> Nat

foldNat :: a -> (a -> a) -> Nat -> a
foldNat zero succ Zero = zero
foldNat zero succ (Succ a) = succ $ foldNat zero succ a

isZero :: Nat -> Bool
isZero = foldNat True (const False)

add, mul :: Nat -> Nat -> Nat
add a = foldNat a Succ
mul a = foldNat Zero (add a)

isEven, isOdd :: Nat -> Bool
isEven a = foldNat True not a
isOdd a = foldNat False not a

unfoldList :: (b -> Maybe (a, b)) -> b -> [a]
unfoldList f z = loop (f z) where
    loop Nothing = []
    loop (Just (a, b')) = a : (loop $ f b')

unfoldNat :: (a -> Maybe a) -> a -> Nat
unfoldNat f a = case (f a) of
    Just b -> Succ (unfoldNat f b)
    Nothing -> Zero