import Ast
import Parser
import World
import qualified Data.Map as M

exec :: World -> Statement -> (Computation (Maybe Float), World)
exec world (Expr expr) = (Just `fmap` evaluate world expr, world)
exec world (Assignment name expr) = case evaluate world expr of
  Failure err -> (Failure err, world)
  Success val -> (Success Nothing, M.insert name (Value val) world)

process :: World -> IO ()
process world = do
  putStr "> "
  input <- getLine
  case parseAst input of
    Left err -> do
      putStrLn "Parsing error:"
      putStrLn $ concat $ map (\s -> "  " ++ s ++ "\n") (lines (show err))
      process world
    Right stmt ->
      let (comp, world') = exec world stmt in
      case comp of
        Failure msg -> print msg >> process world'
        Success (Just val) -> print val >> process world'
        Success Nothing -> process world'
  
main = process caladan
m = main