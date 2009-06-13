module Ast (
  Expr(..),
  BinaryOp(..),
  Statement(..),
  Binding,
  reduce,
  substitute,
  variables,
  grounded
) where

import qualified Data.Map as M
import Data.List (nub)
import Control.Applicative ((<$>))

type Binding = M.Map String Float

data BinaryOp = Add
              | Sub
              | Mul
              | Div
              | Exp
  deriving (Eq, Show)

data Expr = Number Float
          | Variable String
          | Binary BinaryOp Expr Expr
  deriving (Eq)
  
data Statement = Expr Expr
               | Assignment String Expr
  deriving (Eq)

instance Show Statement where
  show (Expr expr)           = show expr
  show (Assignment var expr) = var ++ " = " ++ show expr

instance Show Expr where
  show (Binary op a b) = "(" ++ show a ++ " " ++ symbol op ++ " " ++ show b ++ ")"
  show (Number n) = show n
  show (Variable v) = v

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

lift :: (Float -> Float -> Float) -> Expr -> Expr -> Float
lift op a b = reduce a `op` reduce b

apply :: BinaryOp -> Expr -> Expr -> Float
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
  
showOp :: (Show s) => String -> s -> s -> String
showOp op a b = "(" ++ show a ++ " " ++ op ++ " " ++ show b ++ ")"

reduce :: Expr -> Float
reduce (Binary op a b) = apply op a b
reduce (Number a) = a
reduce (Variable _) = error "Cannot reduce variables"

someExpr :: Expr
someExpr = (3 + 1) * 3 - 1 / 2 + 2 ** 1 ** 2

variables :: Expr -> [String]
variables = nub . nonUnique
  where nonUnique (Number _)     = []
        nonUnique (Binary _ a b) = variables a ++ variables b
        nonUnique (Variable v)   = [v]
        
grounded :: Expr -> Bool
grounded = null . variables

substitute :: Binding -> Expr -> Expr
substitute _       n@(Number _)      = n
substitute binding (Binary op a b)   = Binary op (substitute binding a) (substitute binding b)
substitute binding v@(Variable name) = maybe v Number (M.lookup name binding)

main = do
  print $ show someExpr
  print $ reduce someExpr
  print $ variables (Variable "Foo" + Variable "Foo" + 1)
  print $ substitute (M.fromAscList [("a", 1)]) (Variable "a" + Variable "b")
