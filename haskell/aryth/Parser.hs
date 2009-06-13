module Parser (
  parseAst
) where

import Ast
import Data.List (intercalate)
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
comma      = P.comma lexer
braces     = P.braces lexer

statement = (try definition >>= either fail return)
            <|> try assignment
            <|> liftM Expr expr

assignment = do
  name <- identifier
  reservedOp "="
  value <- expr
  return $ Assignment name value
  
call = do
  name <- identifier
  args <- parens (sepBy expr comma)
  return $ Call name args

definition = do
  name <- identifier
  args <- parens (sepBy identifier comma)
  body <- braces expr
  return $ build name args body

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
    "1 + 2",
    "f(a,b)",
    "f(c, d) { c + d }",
    "f(a, a) { 1 }",
    "f(a) { 1 }",
    "f(x) { x + c }"
  ]

main = do
  forM codes $ \code -> do
    case parseAst code of
      Left err -> putStrLn $ code ++ ": " ++ intercalate " | " (lines (show err))
      Right ast -> putStrLn $ code ++ ": " ++ show ast
  return ()
