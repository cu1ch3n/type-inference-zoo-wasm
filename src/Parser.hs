{-# LANGUAGE ImportQualifiedPost #-}

module Parser (parseTrm, parseTyp) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Lib ()
import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Unbound.Generics.LocallyNameless (Bind, bind, s2n)

type Parser = Parsec Void String

parseTrm :: String -> Either String Trm
parseTrm s =
  case runParser (whole trm) "" s of
    Left err -> Left $ "Parse error: " ++ errorBundlePretty err
    Right e -> Right e

-- | Top-level parsers (should consume all input)
whole :: Parser a -> Parser a
whole p = sc *> p <* eof

------------------------------------------------------------------------
-- Expressions
------------------------------------------------------------------------

trm :: Parser Trm
trm = makeExprParser appTrm trmOps

trmOps :: [[Operator Parser Trm]]
trmOps =
  [ [ InfixL (BinOp OpAdd <$ symbol "+"),
      InfixL (BinOp OpSub <$ symbol "-")
    ],
    [ InfixL (BinOp OpMul <$ symbol "*"),
      InfixL (BinOp OpDiv <$ symbol "/")
    ]
  ]

appTrm :: Parser Trm
appTrm = postfixChain factor (try tapp <|> app)

app :: Parser (Trm -> Trm)
app = do
  e <- factor
  return (`App` e)

tapp :: Parser (Trm -> Trm)
tapp = do
  symbol "@"
  t <- typ
  return (`TApp` t)

factor :: Parser Trm
factor = postfixChain atom annOp

annOp :: Parser (Trm -> Trm)
annOp = do
  symbol ":"
  t <- typ
  return (`Ann` t)

atom :: Parser Trm
atom =
  choice
    [ trmBind Lam $ symbol "\\",
      tLamB,
      letRec,
      letExp,
      ifExp,
      try tuple,
      Var . s2n <$> identifier,
      LitBool True <$ rword "True",
      LitBool False <$ rword "False",
      LitInt <$> int,
      parens trm
    ]

trmBind :: (Bind TmVar Trm -> Trm) -> Parser () -> Parser Trm
trmBind c p = do
  p
  x <- identifier
  symbol "."
  c . bind (s2n x) <$> trm

tLamB :: Parser Trm
tLamB = do
  symbol "/\\"
  (x, b) <- idBound
  symbol "."
  e <- trm
  return $ TLamB (bind (s2n x) e) b

letExp :: Parser Trm
letExp = do
  rword "let"
  x <- identifier
  symbol "="
  e1 <- trm
  rword "in"
  Let e1 . bind (s2n x) <$> trm

letRec :: Parser Trm
letRec = do
  rword "letrec"
  x <- identifier
  symbol "="
  e1 <- trm
  rword "in"
  LetRec . bind (s2n x) . (,) e1 <$> trm

ifExp :: Parser Trm
ifExp = do
  rword "if"
  e1 <- trm
  rword "then"
  e2 <- trm
  rword "else"
  If e1 e2 <$> trm

tuple :: Parser Trm
tuple = do
  symbol "("
  first <- trm
  symbol ","
  rest <- trm `sepBy` symbol ","
  symbol ")"
  return $ Tuple (first : rest)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

typ :: Parser Typ
typ = makeExprParser aTyp typOps

typOps :: [[Operator Parser Typ]]
typOps =
  [ [InfixR (TArr <$ symbol "->")],
    [InfixL (TUnion <$ symbol "|")],
    [InfixL (TIntersection <$ symbol "&")]
  ]

aTyp :: Parser Typ
aTyp =
  choice
    [ tAllB,
      tRecursive,
      tLabeled,
      tTranslatedMu,
      TVar . s2n <$> identifier,
      tConst,
      parens typ
    ]

tAllB :: Parser Typ
tAllB = do
  rword "forall"
  (x, b) <- idBound
  symbol "."
  ty <- typ
  return $ TAllB (bind (s2n x) ty) b

tRecursive :: Parser Typ
tRecursive = do
  rword "mu"
  x <- identifier
  symbol "."
  TRecursive . bind (s2n x) <$> typ

tLabeled :: Parser Typ
tLabeled = do
  symbol "{"
  l <- identifier
  symbol ":"
  ty <- typ
  symbol "}"
  return $ TLabeled (s2n l) (bind (s2n l) ty)

tTranslatedMu :: Parser Typ
tTranslatedMu = do
  symbol "["
  symbol "("
  a <- identifier
  symbol ","
  l <- identifier
  symbol ")"
  symbol "."
  ty <- typ
  symbol "]"
  return $ TTranslatedMu (bind (s2n a, s2n l) ty)

tConst :: Parser Typ
tConst =
  choice
    [ TInt <$ rword "Int",
      TBool <$ rword "Bool",
      TTop <$ rword "Top",
      TBot <$ rword "Bot"
    ]

------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser ()
symbol = void . L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

int :: Parser Integer
int = lexeme L.decimal

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

postfixChain :: Parser a -> Parser (a -> a) -> Parser a
postfixChain p op = do
  x <- p
  rest x
  where
    rest x =
      ( do
          f <- op
          rest $ f x
      )
        <|> return x

rws :: [String]
rws = ["Int", "Bool", "let", "letrec", "in", "fix", "True", "False", "if", "then", "else", "forall", "Top", "Bot", "mu"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> lowerChar <*> many identChar
    check x =
      if x `elem` rws
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

identChar :: Parser Char
identChar = alphaNumChar <|> oneOf "_'"

idBound :: Parser (String, Typ)
idBound = try explicit <|> implicit
  where
    implicit = do
      x <- identifier
      return (x, TTop)
    explicit = do
      symbol "("
      x <- identifier
      symbol "<:"
      t <- aTyp
      symbol ")"
      return (x, t)

parseTyp :: String -> Either String Typ
parseTyp s =
  case runParser (whole typ) "" s of
    Left err -> Left $ "Parse error: " ++ errorBundlePretty err
    Right e -> Right e
