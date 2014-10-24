module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
-- Questions from lecture
-- Reimplementing standard control features
------------------------------------------------------------

-- (*>) :: Applicative f => f a -> f b -> f b
-- (*>) = liftA2 (const id)

-- sequenceA  :: Applicative f => [f a] -> f [a]
-- sequenceA = foldr (liftA2 (:)) (pure [])

-- mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
-- mapA g = sequenceA . map g

-- replicateA :: Applicative f => Int -> f a -> f [a]
-- replicateA n = sequenceA . replicate n

------------------------------------------------------------
-- Homework
------------------------------------------------------------

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> first <*> rest 
  where first = satisfy isAlpha
        rest  = (zeroOrMore $ satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseSExpr :: Parser SExpr
parseSExpr = trim parse
  where trim f     = spaces *> f <* spaces
        parse      = parseAtom <|> parseS
        parseAtom  = A <$> (parseInt <|> parseIdent)
        parseInt   = N <$> posInt   
        parseIdent = I <$> ident
        parseS     = Comb <$> (char '(' *> oneOrMore parseSExpr <* char ')' )
