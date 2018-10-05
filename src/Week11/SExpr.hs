module Week11.SExpr where

import Prelude hiding (lex)
import Control.Applicative
import Data.Char (isSpace, isAlpha, isAlphaNum)
import Week11.AParser (Parser, satisfy, char, posInt)

------------------------------------------------------------
--  Exercise 1: Parsing repetitions
------------------------------------------------------------
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure[]

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  Exercise 2: Utilities
------------------------------------------------------------
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  Exercise 3: Parsing S-expressions
------------------------------------------------------------
-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom
  = N Integer
  | I Ident
  deriving (Show, Eq)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr
  = A Atom
  | Comb [SExpr]
  deriving (Show, Eq)

lex :: Parser a -> Parser a
lex p = p <* spaces

parseAtom :: Parser Atom
parseAtom = lex (N <$> posInt <|> I <$> ident)

parseComb :: Parser [SExpr]
parseComb = lex (char '(') *> oneOrMore (lex parseSExpr) <* lex (char ')')

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (A <$> parseAtom <|> Comb <$> parseComb)
