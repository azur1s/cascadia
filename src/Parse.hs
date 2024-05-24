{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Parse where

import Syntax

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Text.Lazy
import qualified Text.Parsec.Token as P
import qualified Data.Text.Lazy    as L
import qualified Text.Parsec.Expr  as Ex

reservedNames :: [String]
reservedNames = [
    "let",
    "in",
    "rec",
    "if",
    "then",
    "else"
  ]

reservedBin :: [String]
reservedBin = [
    "->",
    "\\",
    "+",
    "&&",
    "==",
    "="
  ]

lexer :: P.GenTokenParser L.Text () Identity
lexer = P.makeTokenParser $ P.LanguageDef
    { P.commentStart    = "{-"
    , P.commentEnd      = "-}"
    , P.commentLine     = "--"
    , P.nestedComments  = True
    , P.identStart      = letter
    , P.identLetter     = alphaNum <|> oneOf "_'"
    , P.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , P.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , P.reservedNames   = reservedNames
    , P.reservedOpNames = reservedBin
    , P.caseSensitive   = True
    }

reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
ident      = P.identifier lexer
parens     = P.parens lexer
semi       = P.semi lexer

contents :: Parser a -> Parser a
contents p = do
    P.whiteSpace lexer
    r <- p
    eof
    return r

integer :: Parser Integer
integer = P.natural lexer

exprInt :: Parser Expr
exprInt = Lit . LInt . fromIntegral <$> integer

exprBool :: Parser Expr
exprBool = (reserved "true"  >> return (Lit $ LBool True))
       <|> (reserved "false" >> return (Lit $ LBool False))

exprVar :: Parser Expr
exprVar = Var <$> ident

exprLam :: Parser Expr
exprLam = do
    reservedOp "\\"
    vars <- many ident
    reservedOp "->"
    Lam (map (, Nothing) vars) <$> expr

exprIf :: Parser Expr
exprIf = do
    reserved "if"
    c <- expr
    reserved "then"
    t <- expr
    reserved "else"
    If c t <$> expr

atom :: Parser Expr
atom = parens expr
    <|> exprInt
    <|> exprBool
    <|> exprVar
    <|> exprLam
    <|> exprIf

term :: Parser Expr
term = atom >>= \x -> (many1 atom >>= \xs -> return $ App x xs)
    <|> return x

expr :: Parser Expr
expr = Ex.buildExpressionParser table term
    where
        table = [
            [ unary  "-"  (Una "-")
            , unary  "!"  (Una "!")
            ],
            [ binary "*"  (Bin "*")
            , binary "/"  (Bin "/")
            , binary "%"  (Bin "%")
            ],
            [ binary "+"  (Bin "+")
            , binary "-"  (Bin "-")
            ],
            [ binary "==" (Bin "==")
            , binary "!=" (Bin "!=")
            , binary "<"  (Bin "<")
            , binary "<=" (Bin "<=")
            , binary ">"  (Bin ">")
            , binary ">=" (Bin ">=")
            ],
            [ binary "&&" (Bin "&&")
            , binary "||" (Bin "||")
            ]
          ]
        unary  name f = Ex.Prefix (reservedOp name >> return f)
        binary name f = Ex.Infix (reservedOp name >> return f) Ex.AssocLeft

decl :: Parser Top
decl = do
    name <- ident
    args <- many ident
    reservedOp "="
    body <- expr
    _ <- semi
    case args of [] -> return $ Decl name body
                 _  -> return $ Decl name $ Lam (map (, Nothing) args) body

top :: Parser Top
top = decl

modl :: Parser [Top]
modl = many top

runParser :: String -> String -> Either ParseError [Top]
runParser src path = parse (contents modl) path $ L.pack src
