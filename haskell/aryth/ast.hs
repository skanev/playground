module Ast (
  AST(..), reduce
) where

import Control.Applicative ((<$>))

data AST =
  Number Float
  | Add AST AST
  | Mul AST AST
  deriving (Eq)

instance Show AST where
  show (Add a b) = showOp "+" a b
  show (Mul a b) = showOp "*" a b
  show (Number a) = show a

instance Num AST where
  (+) = Add
  (*) = Mul
  abs = undefined
  signum = undefined
  fromInteger = Number . fromIntegral
  
showOp :: (Show s) => String -> s -> s -> String
showOp op a b = "(" ++ show a ++ " " ++ op ++ " " ++ show b ++ ")"

reduce :: AST -> Float
reduce (Add a b) = reduce a + reduce b
reduce (Mul a b) = reduce a * reduce b
reduce (Number a) = a





twelve :: AST
twelve = (3 + 1) * 3

main = print $ show twelve