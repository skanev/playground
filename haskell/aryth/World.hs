module World (
  World,
  Inhabitant(..),
  Computation,
  ComputationError(..),
  evaluate,
  caladan
) where
  
import Ast
import qualified Data.Map as M
import Control.Monad (liftM2, mapM)
import Control.Monad.Error (Error(..), throwError)

data FuncDef = FuncDef { args :: [String], body :: Expr } deriving (Eq, Show)

data Inhabitant = Value Float
                | Function FuncDef
  deriving (Eq, Show)

type World = M.Map String Inhabitant

newtype ComputationError = ComputationError { msgOf :: String } deriving (Show)

instance Error ComputationError where
  noMsg  = ComputationError "What the phukk?"
  strMsg = ComputationError 

type Computation = Either ComputationError

evaluate :: World -> Expr -> Computation Float
evaluate world (Number n) = return n
evaluate world (Binary op a b) = liftM2 (apply op) (evaluate world a) (evaluate world b)
evaluate world (Name name) = maybe err return (value world name)
  where err = throwError (strMsg $ "Undefined variable `" ++ name ++ "'")
evaluate world (Call name exprs) = do
    FuncDef args body <- maybe undefinedFunc return (function world name)
    params <- evaluate world `mapM` exprs
    funcWorld <- bind args params
    evaluate funcWorld body
  where undefinedFunc = throwError (strMsg $ "Undefined function `" ++ name ++ "'")

bind :: [String] -> [Float] -> Computation World
bind names values
  | length names == length values = return $ M.fromList (zip names (map Value values))
  | otherwise                     = throwError (strMsg $ "Supplied arguments " ++ show values ++ " for " ++ show names)
  

value :: World -> String -> Maybe Float
value world name = do
  Value val <- M.lookup name world
  return val
  
function :: World -> String -> Maybe FuncDef
function world name = do
  Function def <- M.lookup name world
  return def

apply :: BinaryOp -> Float -> Float -> Float
apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Div = (/)
apply Exp = (**)

caladan :: World
caladan = M.fromList [
    ("paul", Value 3.14),
    ("jessica", Value 2.71), 
    ("larodi", Function (FuncDef [] (Number 1))),
    ("add", Function (FuncDef ["a", "b"] (Name "a" + Name "b")))
  ]

main = do
  print $ evaluate caladan (1 + 2)
  print $ evaluate caladan (1 + 2 + Name "paul")
  print $ evaluate caladan (1 + 2 + Name "larodi")
  print $ evaluate caladan (Call "foo" [1.4, 2])
  print "---->"
  print $ evaluate caladan (Call "add" [Name "jessica", 2])
  print $ evaluate caladan (Call "larodi" [])
  return ()

