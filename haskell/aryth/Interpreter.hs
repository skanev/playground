import Ast
import Parser
import qualified Data.Map as M

closedIn :: Expr -> Binding -> Bool
expr `closedIn` binding = grounded (substitute binding expr)

valueIn :: Expr -> Binding -> Maybe Float
valueIn expr binding
  | expr `closedIn` binding = Just $ reduce (substitute binding expr)
  | otherwise               = Nothing              

getExpr :: Statement -> Expr
getExpr (Expr expr)         = expr
getExpr (Assignment _ expr) = expr

exec :: Binding -> Statement -> IO Binding
exec binding stmt = case expr `valueIn` binding of
    Just value -> case stmt of
      Expr expr -> print value >> return binding
      Assignment name expr -> return $ M.insert name value binding
    Nothing -> do
      putStrLn $ "Undefined variables: " ++ show (variables (substitute binding expr))
      return binding
  where expr = getExpr stmt

process :: Binding -> IO ()
process binding = do
  putStr "> "
  input <- getLine
  case parseAst input of
    Left err -> do
      putStrLn "Parsing error:"
      putStrLn $ concat $ map (\s -> "  " ++ s ++ "\n") (lines (show err))
      process binding
    Right stmt -> exec binding stmt >>= process
  
main = process M.empty
m = main