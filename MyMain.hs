module MyMain where

{-
    module Types(
        State(..), Reader(..), Writer(..),  - export with methods
        runState, runWriter, runReader,     - export methods
        module Data.Monoid) where           - export module
-}

{-
    -- imports
    import Prelude()                            -- import nothing from Prelude
    import Prelude(Bool(..), Show(..), Eq(..))  -- import specified types and all ctors/methods
    import Prelude(..., (&&))                   -- import method
    import qualified Prelude as P( ... )        -- qualified import
    import Prelude hiding (id,(.))              -- hide
-}
import Prelude(Bool(..), Show(..), {-Eq(..),-} String(..), Int(..), Double(..), (&&), not, (+), (-), (++), (*), (/), sqrt)
import Sub.SubEmpty
import qualified Sub.Stream as S
import qualified Sub.SpModule as P

{-
    -- decalration of functions
    ifThenElse :: Bool -> a -> a -> a   -- declaration
    ifThenElse True  a _ = a
    ifThenElse False _ b = b

    t :: Bool
    t = True

    data MyData = My | Data deriving(Show, Eq) -- default implementation of classes
-}

{-
    -- classes
    class MyEq a where
        (==) :: a -> a -> Bool
        (/=) :: a -> a -> Bool

    -- Default implementation
        a == b = not (a /= b)
        a /= b = not (a == b)
    -- Or
        (==) a b = not (a /= b)
        (/=) a b = not (a == b)

    -- instance
    instance MyEq Foo where 
        show foo = ...
-}

{-
    -- if then else clause
    foo :: String
    foo = if True then "t" else "false"
-}  

{-
    -- tuple
    tupledFoo :: (String, Int) -> String
    tupledFoo (a, 1) = a
    tupledFoo (a, n) = a ++ tupledFoo (a, n - 1)
-}

{-
    -- sum of types
    data Num = Zero | Succ Num

    -- mult of types
    data Date = Date Int Int Int

    -- name starts from :
    data MyType = Int :++ MyType

    -- Type with named fields
    data Passport = Person {
        surname :: String,
        givenName :: String
    } deriving (...)

    -- Another example of type declaration
    -- Also ctors started with : place between args a:[b,c,d]
    data [a] where
        [] :: [a]
        (:) :: a -> [a] -> [a]

    -- Field access
    hello :: Passport -> String
    hello p = "Hello, " ++ givenName p ++ "!"

    -- Field change
    newP = p{surname = "qweqwe"}


    -- Parametrized types
    data Maybe a = Nothing | Just a
    data ParamRecord a b c = ParamRecord {p1 :: a, p2 :: b, p3 :: c}

    data (Ord k) => Map k v = ...
    data Vector a = Vector a a a


    data MySuperType a = 
        AAA a 
        | BBB 
        | CCC 
        | MySuperType a Int 
        | SomeRecord { aaa:: Int} 
        deriving(Show)
-}

{-
    -- Declarative style
    square :: Double -> Double -> Double -> Double
    square a b c = sqrt(p * (pa) * (pb) * (pc)) where
        p = (a + b + c) / 2
        pa = p - a
        pb = p - b
        pc = p - c

    isEvenLen :: [a] -> Bool
    isEvenLen l = loop l where
        loop []             = True
        loop (_ : _ : subl) = loop subl
        loop _              = False 

    -- Compositional
    square :: Double -> Double -> Double -> Double
    square a b c = let  p = (a + b + c) / 2
                        pa = p - a
                        pb = p - b
                        pc = p - c
                    in sqrt(p * pa * pb * pc)
-}

{-
    -- Named pattern matching
    beside :: Nat -> (Nat, Nat)
    beside Zero         = error ”undefined”
    beside x@(Succ y)   = (y, Succ x)
-}

{-
    -- case
    toAnother :: Nat -> AnotherNat
    toAnother x =
        case x of
            Zero -> None
            Succ Zero -> One
            Succ (Succ Zero) -> Two
            _ -> Many
-}

{-
    -- guard expressions
    filter :: (a -> Bool) -> [a] -> [a]
    filter p [] = []
    filter p (x:xs)
        | p x = x : rest
        | otherwise = rest    -- alias for True
        where rest = filter p xs
-}

{-
    -- Lambda
    f :: [Int] -> [Int]
    f = filter (\x -> x > 2 && x < 10 && even x)

    \x y -> ...     -- from two params
    \(x, y) -> ...  -- from tuple
-}

{-
    -- operator priority
    infixr 0 +$, *$

    -- class requirements
    (*$) :: Kleisli m => (a -> m b) -> m a -> m b
-}


{-
    -- Type alias (you can pass double instead of alias)
    type Velocity = Double
    type Time = Double
    type Length = Double

    -- Type wrapper (you can pass only wrapped type (not pure double))
    -- You can export only method that returns newtype, instead ov raw newtype
    newtype Velocity = Velocity Double
    newtype Time = Time Double
    newtype Length = Length Double

    -- dont export it
    newtype Sorted a = [a]
    -- but export function
    sorted :: [a] -> Sorted a

    -- also you can use newtype to control monoid combine method implementation
    newtype Sum = Sum Int
    newtype Product = Product Int

-}

{-
    data Nat = Zero | Succ Nat

    data Nat where
        Zero :: Nat
        Succ :: Nat -> Nat
-}

{-
    -- special functions 
    (.) :: (a -> b) -> (b -> c) -> (a -> c)     -- composition
    ($) :: (a -> b) -> a -> b       -- foo $ param - operator with low priority
                                    -- see infixl infixr
-}

{-
    -- For all statement [TODO]
    forall a.
-}

{-
    -- Do-monad sugar
    main = do  
        putStrLn "What's your first name?"  
        firstName <- getLine  
        putStrLn "What's your last name?"  
        lastName <- getLine  
        let bigFirstName = map toUpper firstName  
            bigLastName = map toUpper lastName  
        putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?" 
-}

{-
    instance Applicative ((->) r) where  
    (->) a b    <=>  a -> b
    applicative requires f[_]
-}