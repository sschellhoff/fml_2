 {-# LANGUAGE OverloadedStrings #-}
module FMLParser where
import Ast

import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.Combinators.Expr as CE
import qualified Ast
import qualified Ast
import qualified Ast
import qualified Ast
import qualified Ast
import qualified Ast
import qualified Ast

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1                        
  (L.skipLineComment "//")      
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

parseKeyword :: Text -> Parser Text
parseKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

reservedWords :: [String]
reservedWords = ["const", "let", "while", "true", "false"]

parseIdentifier :: Parser String
parseIdentifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parseIntConst :: Parser Ast.ParsedExpr
parseIntConst = Ast.IntConst () <$> lexeme L.decimal

parseFloatConst :: Parser Ast.ParsedExpr
parseFloatConst = Ast.FloatConst () <$> lexeme L.float

parseNumberConst :: Parser Ast.ParsedExpr
parseNumberConst = try parseFloatConst <|> parseIntConst

parseBooleanConst :: Parser Ast.ParsedExpr
parseBooleanConst = do
    value <- parseKeyword "true" <|> parseKeyword "false"
    return (Ast.BoolConst () (value == "true"))

parseVariable :: Parser Ast.ParsedExpr
parseVariable = do
    name <- parseIdentifier
    return (Ast.Var () name)

parseTerm :: Parser Ast.ParsedExpr
parseTerm = choice
    [ parens parseExpr
    , parseNumberConst
    , parseBooleanConst
    , parseVariable
    ]

binary :: Text -> (Ast.ParsedExpr -> Ast.ParsedExpr -> Ast.ParsedExpr) -> CE.Operator Parser Ast.ParsedExpr
binary  name f = CE.InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (Ast.ParsedExpr -> Ast.ParsedExpr) -> CE.Operator Parser Ast.ParsedExpr
prefix  name f = CE.Prefix  (f <$ symbol name)
postfix name f = CE.Postfix (f <$ symbol name)

operatorTable :: [[CE.Operator Parser Ast.ParsedExpr]]
operatorTable =
  [ [ prefix "-" (Ast.toParsedPrefixExpr Ast.OpNeg)
    , prefix "!" (Ast.toParsedPrefixExpr Ast.OpNot)
    , prefix "+" id
    ]

  , [ binary "*" (Ast.toParsedInfixExpr Ast.InfixMult)
    , binary "/" (Ast.toParsedInfixExpr Ast.InfixDiv)
    , binary "%" (Ast.toParsedInfixExpr Ast.InfixMod)
    ]
  , [ binary "+" (Ast.toParsedInfixExpr Ast.InfixAdd)
    , binary "-" (Ast.toParsedInfixExpr Ast.InfixSub)
    ]
  , [ binary "<" (Ast.toParsedInfixExpr Ast.InfixLt)
    , binary ">" (Ast.toParsedInfixExpr Ast.InfixGt)   
    , binary ">=" (Ast.toParsedInfixExpr Ast.InfixGe) 
    , binary "<=" (Ast.toParsedInfixExpr Ast.InfixLe) 
    ] 
  , [ binary "==" (Ast.toParsedInfixExpr Ast.InfixEq)
    , binary "!=" (Ast.toParsedInfixExpr Ast.InfixNeq)
    ]
  , [ binary "&&" (Ast.toParsedInfixExpr Ast.InfixAnd)
    ]
  , [ binary "||" (Ast.toParsedInfixExpr Ast.InfixOr)
    ]
  ]

parseExpr :: Parser Ast.ParsedExpr
parseExpr = CE.makeExprParser parseTerm operatorTable

parseStmt :: Parser Ast.ParsedAst
parseStmt = choice
    [ parseWhileStmt
    , parseConstStmt
    , try parseAssignStmt
    , parseLetStmt
    , parseExprStmt
    ]

parseConstStmt :: Parser Ast.ParsedAst
parseConstStmt = do
    _ <- parseKeyword "const"
    name <- parseIdentifier
    _ <- symbol "="
    expr <- parseExpr
    return (Ast.ConstDecl () name expr)

parseAssignStmt :: Parser Ast.ParsedAst
parseAssignStmt = do
    name <- parseIdentifier
    _ <- symbol "="
    expr <- parseExpr
    return (Ast.Assign () name expr)

parseLetStmt :: Parser Ast.ParsedAst
parseLetStmt = do
    _ <- parseKeyword "let"
    name <- parseIdentifier
    _ <- symbol "="
    expr <- parseExpr
    return (Ast.ConstDecl () name expr)

parseExprStmt :: Parser Ast.ParsedAst
parseExprStmt = do
    expr <- parseExpr
    return (Ast.ExprStmt () expr)

parseStmts :: Parser [Ast.ParsedAst]
parseStmts = many parseStmt

parseBlock :: Parser [Ast.ParsedAst]
parseBlock = do
    stmts <- between (symbol "{") (symbol "}") parseStmts
    return (stmts)

parseWhileStmt :: Parser Ast.ParsedAst
parseWhileStmt = do
    _ <- parseKeyword "while"
    condition <- parseExpr
    block <- parseBlock
    return (Ast.While () condition block)

parseFile content = parseTest (parseStmt <* eof) (pack content) -- TODO read fileinput and parse the content

test :: IO ()
test = do
    parseTest (parseExpr <* eof) "1 + 2 * 3"
    parseTest (parseStmt <* eof) "const a = 13 + 37"
    parseTest (parseStmt <* eof) "let b = 13 + 37"
    parseTest (parseStmt <* eof) "b = 42"
    parseTest (parseStmt <* eof) "13 * 37 + 42"
