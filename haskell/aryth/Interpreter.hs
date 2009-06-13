import Ast
import Parser

mainx = case parseAst "1 + 2 * 3 ^ 4" of
  Left err -> print err
  Right ast -> print (reduce ast)

process = do
  putStr "> "
  input <- getLine
  case parseAst input of
    Left err -> do
      putStrLn "Parsing error:"
      putStrLn $ concat $ map (\s -> "  " ++ s ++ "\n") (lines (show err))
    Right ast -> do
      print (reduce ast)
  process
  
main = process
m = main