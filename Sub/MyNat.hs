module Sub.MyNat where

import Data.Function(fix)

data MyNum = Zero | Succ MyNum deriving(Show)

-- data MyNum where
--     Zero :: MyNum
--     Succ :: MyNum -> MyNum


instance Num MyNum where 
    (+) a Zero      = a
    (+) a (Succ b)  = Succ(a + b)

    (-) a b = error "err"
    
    (*) _ Zero      = Zero
    (*) a (Succ b)  = a + (a * b)
    
    abs a       = a
    signum a    = a

    fromInteger 0 = Zero
    fromInteger a = Succ $ fromInteger(a - 1)

foldNat :: a -> (a -> a) -> MyNum -> a
foldNat z _ Zero = z
foldNat z f (Succ n) = f $ foldNat z f n



foldNat' :: a -> (a -> a) -> MyNum -> a
foldNat' z f n = foo z f n where
    foo :: a -> (a -> a) -> MyNum -> a
    foo z _ Zero = z
    foo z f (Succ n) = foo (f z) f n


foldNatF :: (a -> a) -> MyNum -> a -> a
foldNatF _ Zero = id
foldNatF f n = foo f id n where
    foo :: (a -> a) -> (a -> a) -> MyNum -> a -> a
    foo incf f' Zero = f'
    foo incf f' (Succ n) = foo incf (incf . f') n

tfn :: [Int]
tfn = foldNat [] (1:) (Succ(Succ(Succ(Zero))))

tfn' :: [Int]
tfn' = foldNat [] (1:) (Succ(Succ(Succ(Zero))))

tfnf' = foldNatF (1:) (Succ(Succ(Succ(Zero))))


instance (Num b) => Num (a -> b) where 
    (+) a b = \t -> a t + b t
    (-) a b = error "err"
    (*) a b = \t -> a t * b t
    
    abs a       = \t -> abs $ a t
    signum a    = \t -> signum $ a t

    fromInteger a = (\x y -> x) (fromInteger a)  -- const . fromInteger


nf = (\x -> x) + (\x -> x + 5)

fromIntTest = (55 + 66) 99

-- f(x,y) = x^2 + y^2
-- f :: a -> b -> c
-- x :: a -> a -> a
-- y :: a -> a -> a
-- x = \x y -> x
x = const id
-- y = \x y -> y
y = flip $ const id
-- 
f = x * x + y * y


-- fix :: (a -> a) -> a
-- fix f = f(fix f)
-- fix f = f(f(f(f(f(...)))))
--
-- f :: (a -> a) -> (a -> a)
-- fix f :: ((a -> a) -> a -> a) -> a -> a
foldNatFix :: a -> (a -> a) -> MyNum -> a
foldNatFix z _ Zero = z
foldNatFix z f n = fix recf n where 
    recf rest nat = case nat of
        Zero -> z
        Succ n' -> f (rest n') 