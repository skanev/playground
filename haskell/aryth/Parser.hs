module Parser (
  parseAst
) where

import Ast
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr (buildExpressionParser, Assoc(..), OperatorTable(..), Operator(..))
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Control.Monad (liftM, forM)

lexer      = P.makeTokenParser emptyDef

whiteSpace = P.whiteSpace lexer
natural    = P.natural lexer
parens     = P.parens lexer
reservedOp = P.reservedOp lexer
identifier = P.identifier lexer

statement = try assignment
            <|> liftM Expr expr

assignment = do
  name <- identifier
  reservedOp "="
  value <- expr
  return $ Assignment name value
  

call = do
  name <- identifier
  args <- parens (sepBy expr (reservedOp ","))
  return $ Call name args

expr = buildExpressionParser table term
        <?> "expression"

term = try call
        <|> parens expr
        <|> liftM (Number . fromIntegral) natural
        <|> liftM Name identifier
        <?> "simple expression"

table :: OperatorTable Char () Expr
table = [ [binary "^" Exp AssocRight],
          [binary "*" Mul AssocLeft, binary "/" Div AssocLeft],
          [binary "+" Add AssocLeft, binary "-" Sub AssocLeft]
        ]

binary symbol op assoc = Infix (reservedOp symbol >> return (Binary op)) assoc

parseAst input = parse parser "(ast input)" input 
  where parser = do
          whiteSpace
          ast <- statement
          eof
          return ast

codes = [
  ]

main = do
  forM codes $ \code -> do
    case parse statement "(Dr. Who)" code of
      Left err -> print err
      Right ast -> putStrLn $ code ++ ": " ++ show ast
  return ()
