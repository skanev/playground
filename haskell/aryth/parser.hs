module Parser (
  parseAst
) where

import Ast
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr (buildExpressionParser, Assoc(..), OperatorTable(..), Operator(..))
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Control.Monad (liftM)

lexer      = P.makeTokenParser emptyDef

whiteSpace = P.whiteSpace lexer
natural    = P.natural lexer
parens     = P.parens lexer
reservedOp = P.reservedOp lexer

expr = buildExpressionParser table term
        <?> "expression"

term = parens expr
        <|> liftM (Number . fromIntegral) natural
        <?> "simple expression"

table :: OperatorTable Char () AST
table = [ [binary "^" Exp AssocRight],
          [binary "*" Mul AssocLeft, binary "/" Div AssocLeft],
          [binary "+" Add AssocLeft, binary "-" Sub AssocLeft]
        ]

binary name fun assoc = Infix (do{ reservedOp name; return fun }) assoc

parseAst input = parse parser "(ast input)" input 
  where parser = do
          whiteSpace
          ast <- expr
          eof
          return ast

main = case parse expr "(Dr. Who)" "(4 / 2 - 1) * 2 - 1 + 2 ^ 3 ^ 4" of
  Left err -> print err
  Right ast -> do
    print ast
    print (reduce ast)
