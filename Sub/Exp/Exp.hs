{-# LANGUAGE DataKinds #-}

module Sub.Exp.Exp where

import Control.Monad.Reader
import Control.Applicative
import qualified Data.Map as M

data Exp    = Lit Int 
            | Add Exp Exp
            | Mul Exp Exp
            | Neg Exp
            | Var String
            deriving (Show)

type ExprEnv = M.Map String Int

eval :: Exp -> Reader ExprEnv Int
eval (Lit a) = pure a
eval (Add l r) = liftA2 (+) (eval l) (eval r)
eval (Mul l r) = liftA2 (*) (eval l) (eval r)
eval (Neg a) = liftA negate $ (eval a)
eval (Var n) = reader $ \env -> value env n

value :: ExprEnv -> String -> Int
value env k = env M.! k

var :: String -> Exp
var = Var

n :: Show a => a -> Exp
n = Var . show

instance Num Exp where
    negate = Neg
    (+)    = Add
    (*)    = Mul
    abs    = undefined
    signum = undefined
    fromInteger = Lit . fromInteger 


testEnv :: ExprEnv
testEnv = M.fromList[("qwe", 1), ("b", 2)]

testExpr :: Exp
testExpr = Add 2 (Mul 2 2) + var "qwe"

testR = runReader (eval testExpr) testEnv




