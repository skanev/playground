module Ast (
  Expr(..),
  BinaryOp(..),
  Statement(..),
) where

import qualified Data.Map as M
import Data.List (nub, intercalate)
import Control.Applicative ((<$>))

data BinaryOp = Add
              | Sub
              | Mul
              | Div
              | Exp
  deriving (Eq, Show)

data Expr = Number Float
          | Name String
          | Call String [Expr]
          | Binary BinaryOp Expr Expr
  deriving (Eq)
  
data Statement = Expr Expr
               | Assignment String Expr
  deriving (Eq)

instance Show Statement where
  show (Expr expr)           = show expr
  show (Assignment var expr) = var ++ " = " ++ show expr

instance Show Expr where
  show (Number n) = show n
  show (Name v) = v
  show (Call name args) = name ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (Binary op a b) = "(" ++ show a ++ " " ++ symbol op ++ " " ++ show b ++ ")"

instance Num Expr where
  (+) = Binary Add
  (*) = Binary Mul
  (-) = Binary Sub
  abs = undefined
  signum = undefined
  fromInteger = Number . fromIntegral

instance Fractional Expr where
  (/) = Binary Div
  fromRational = Number . fromRational
  
instance Floating Expr where
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

symbol :: BinaryOp -> String
symbol Add = "+"
symbol Sub = "-"
symbol Mul = "*"
symbol Div = "/"
symbol Exp = "^"
  
showOp :: (Show s) => String -> s -> s -> String
showOp op a b = "(" ++ show a ++ " " ++ op ++ " " ++ show b ++ ")"

someExpr :: Expr
someExpr = (3 + 1) * 3 - 1 / 2 + 2 ** 1 ** 2

main = do
  print $ show someExpr
  print $ 1 + Call "foo" [1, 2, Name "a" + 1]
