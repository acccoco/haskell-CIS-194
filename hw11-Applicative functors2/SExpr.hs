{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  Exercise 1. Parsing repetitions
------------------------------------------------------------
-- 进行多次匹配，或者零次匹配
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

-- 进行至少一次匹配，否则匹配失败
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = liftA2 (:) p (zeroOrMore p)

------------------------------------------------------------
--  Exercise 2. Utilities
------------------------------------------------------------
-- 匹配开头的零个或多个空白字符
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

-- 陪陪开头的 identifier
ident :: Parser String
ident = liftA2 (:) (satisfy isAlpha) (oneOrMore $ satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving (Show)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr
  = A Atom
  | Comb [SExpr]
  deriving (Show)

------------------------------------------------------------
--  Exercise 3
------------------------------------------------------------
-- 可以识别原子组成部分，要么是整形，要么是 identifier
parseAtom :: Parser Atom
parseAtom = (N <$> posInt) <|> (I <$> ident)

-- 可以识别 SExpr
parseSExpr :: Parser SExpr
parseSExpr =
  spaces
    *> ( (A <$> parseAtom)
           <|> (char '(' *> (Comb <$> oneOrMore parseSExpr) <* char ')')
       )
    <* spaces
