module Ast (
  AST(..), reduce
) where

import Control.Applicative ((<$>))

data AST =
  Number Float
  | Add AST AST
  | Sub AST AST
  | Mul AST AST
  | Div AST AST
  | Exp AST AST
  deriving (Eq)

instance Show AST where
  show (Add a b) = showOp "+" a b
  show (Sub a b) = showOp "-" a b
  show (Mul a b) = showOp "*" a b
  show (Div a b) = showOp "/" a b
  show (Exp a b) = showOp "^" a b
  show (Number a) = show a

instance Num AST where
  (+) = Add
  (*) = Mul
  (-) = Sub
  abs = undefined
  signum = undefined
  fromInteger = Number . fromIntegral

instance Fractional AST where
  (/) = Div
  fromRational = Number . fromRational
  
instance Floating AST where
  (**)  = Exp
  pi    = undefined
  exp   = undefined
  log   = undefined
  sin   = undefined
  cos   = undefined
  asin  = undefined
  atan  = undefined
  acos  = undefined
  sinh  = undefined
  cosh  = undefined
  asinh = undefined
  atanh = undefined
  acosh = undefined
  
showOp :: (Show s) => String -> s -> s -> String
showOp op a b = "(" ++ show a ++ " " ++ op ++ " " ++ show b ++ ")"

reduce :: AST -> Float
reduce (Add a b) = reduce a + reduce b
reduce (Mul a b) = reduce a * reduce b
reduce (Sub a b) = reduce a - reduce b
reduce (Div a b) = reduce a / reduce b
reduce (Exp a b) = reduce a ** reduce b
reduce (Number a) = a

someExpr :: AST
someExpr = (3 + 1) * 3 - 1 / 2 + 2 ** 1 ** 2

main = print $ show someExpr