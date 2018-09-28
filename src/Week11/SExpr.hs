module Week11.SExpr where

import Control.Applicative
import Data.Char (isSpace, isAlpha, isAlphaNum)
import Week11.AParser (Parser, satisfy, char, posInt)

------------------------------------------------------------
--  Exercise 1: Parsing repetitions
------------------------------------------------------------
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = many

oneOrMore :: Parser a -> Parser [a]
oneOrMore = some

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

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (parseAtom <|> parseSExprList)
  where parseAtom = lex' (A <$> (N <$> posInt <|> I <$> ident))
        parseSExprList = lex' $ between (char '(') (char ')') parseComb
        parseComb = Comb <$> oneOrMore parseSExpr
        lex' p = p <* spaces
        between bra ket p = bra *> p <* ket
