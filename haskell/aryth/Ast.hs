module Ast (
  AST(..), BinaryOp(..), reduce
) where

import Control.Applicative ((<$>))

data BinaryOp = Add
              | Sub
              | Mul
              | Div
              | Exp
  deriving (Eq, Show)

data AST =  Number Float
          | Binary BinaryOp AST AST
  deriving (Eq)


lift :: (Float -> Float -> Float) -> AST -> AST -> Float
lift op a b = reduce a `op` reduce b

apply :: BinaryOp -> AST -> AST -> Float
apply Add = lift (+)
apply Sub = lift (-)
apply Mul = lift (*)
apply Div = lift (/)
apply Exp = lift (**)


symbol :: BinaryOp -> String
symbol Add = "+"
symbol Sub = "-"
symbol Mul = "*"
symbol Div = "/"
symbol Exp = "^"

instance Show AST where
  show (Binary op a b) = "(" ++ show a ++ " " ++ symbol op ++ " " ++ show b ++ ")"
  show (Number a) = show a

instance Num AST where
  (+) = Binary Add
  (*) = Binary Mul
  (-) = Binary Sub
  abs = undefined
  signum = undefined
  fromInteger = Number . fromIntegral

instance Fractional AST where
  (/) = Binary Div
  fromRational = Number . fromRational
  
instance Floating AST where
  (**)  = Binary Exp
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
reduce (Binary op a b) = apply op a b
reduce (Number a) = a

someExpr :: AST
someExpr = (3 + 1) * 3 - 1 / 2 + 2 ** 1 ** 2

main = do
  print $ show someExpr
  print $ reduce someExpr