module Lib
    ( eval
    , evalStr
    , Expr(..)
    ) where

import ExprT
import Parser (parseExp)

class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr ExprT where
    lit a = Lit a
    mul a b = Mul a b
    add a b = Add a b

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp Lit Add Mul s
