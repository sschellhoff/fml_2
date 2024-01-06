 {-# LANGUAGE OverloadedStrings #-}
module FMLParser where
import Ast

import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.Combinators.Expr as CE
import Text.ParserCombinators.ReadP (get)

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
parseIntConst = do
  pos <- getOffset
  Ast.IntConst (makeParseInfo pos) <$> lexeme L.decimal

parseFloatConst :: Parser Ast.ParsedExpr
parseFloatConst = do
  pos <- getOffset
  Ast.FloatConst (makeParseInfo pos) <$> lexeme L.float

parseNumberConst :: Parser Ast.ParsedExpr
parseNumberConst = try parseFloatConst <|> parseIntConst

parseBooleanConst :: Parser Ast.ParsedExpr
parseBooleanConst = do
    pos <- getOffset
    value <- parseKeyword "true" <|> parseKeyword "false"
    return (Ast.BoolConst (makeParseInfo pos) (value == "true"))

parseVariable :: Parser Ast.ParsedExpr
parseVariable = do
    pos <- getOffset
    Ast.Var (makeParseInfo pos) <$> parseIdentifier

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

operatorTable :: [[CE.Operator Parser Ast.ParsedExpr]] -- TODO parsePosition is invalid
operatorTable =
  [ [ prefix "-" (Ast.toParsedPrefixExpr Ast.OpNeg (makeParseInfo 0))
    , prefix "!" (Ast.toParsedPrefixExpr Ast.OpNot (makeParseInfo 0))
    , prefix "+" id
    ]

  , [ binary "*" (Ast.toParsedInfixExpr Ast.InfixMult (makeParseInfo 0))
    , binary "/" (Ast.toParsedInfixExpr Ast.InfixDiv (makeParseInfo 0))
    , binary "%" (Ast.toParsedInfixExpr Ast.InfixMod (makeParseInfo 0))
    ]
  , [ binary "+" (Ast.toParsedInfixExpr Ast.InfixAdd (makeParseInfo 0))
    , binary "-" (Ast.toParsedInfixExpr Ast.InfixSub (makeParseInfo 0))
    ]
  , [ binary "<" (Ast.toParsedInfixExpr Ast.InfixLt (makeParseInfo 0))
    , binary ">" (Ast.toParsedInfixExpr Ast.InfixGt (makeParseInfo 0))   
    , binary ">=" (Ast.toParsedInfixExpr Ast.InfixGe (makeParseInfo 0)) 
    , binary "<=" (Ast.toParsedInfixExpr Ast.InfixLe (makeParseInfo 0)) 
    ] 
  , [ binary "==" (Ast.toParsedInfixExpr Ast.InfixEq (makeParseInfo 0))
    , binary "!=" (Ast.toParsedInfixExpr Ast.InfixNeq (makeParseInfo 0))
    ]
  , [ binary "&&" (Ast.toParsedInfixExpr Ast.InfixAnd (makeParseInfo 0))
    ]
  , [ binary "||" (Ast.toParsedInfixExpr Ast.InfixOr (makeParseInfo 0))
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
    pos <- getOffset
    _ <- parseKeyword "const"
    name <- parseIdentifier
    _ <- symbol "="
    Ast.ConstDecl (makeParseInfo pos) name <$> parseExpr

parseAssignStmt :: Parser Ast.ParsedAst
parseAssignStmt = do
    pos <- getOffset
    name <- parseIdentifier
    _ <- symbol "="
    Ast.Assign (makeParseInfo pos) name <$> parseExpr

parseLetStmt :: Parser Ast.ParsedAst
parseLetStmt = do
    pos <- getOffset
    _ <- parseKeyword "let"
    name <- parseIdentifier
    _ <- symbol "="
    Ast.ConstDecl (makeParseInfo pos) name <$> parseExpr

parseExprStmt :: Parser Ast.ParsedAst
parseExprStmt = do
    pos <- getOffset
    Ast.ExprStmt (makeParseInfo pos) <$> parseExpr

parseStmts :: Parser [Ast.ParsedAst]
parseStmts = many parseStmt

parseBlock :: Parser [Ast.ParsedAst]
parseBlock = between (symbol "{") (symbol "}") parseStmts

parseWhileStmt :: Parser Ast.ParsedAst
parseWhileStmt = do
    pos <- getOffset
    _ <- parseKeyword "while"
    condition <- parseExpr
    Ast.While (makeParseInfo pos) condition <$> parseBlock

parseFile content = parseTest (parseStmt <* eof) (pack content) -- TODO read fileinput and parse the content

test :: IO ()
test = do
    parseTest (parseExpr <* eof) "1 + 2 * 3"
    parseTest (parseStmt <* eof) "const a = 13 + 37"
    parseTest (parseStmt <* eof) "let b = 13 + 37"
    parseTest (parseStmt <* eof) "b = 42"
    parseTest (parseStmt <* eof) "13 * 37 + 42"
