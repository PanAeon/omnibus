module Main where

import Lib

import Data.Void
import Control.Monad (void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Arrow(left)
import Control.Applicative(Alternative, liftA2)

data Expression =
     Term Char
   | Many Expression
   | Next Expression Expression -- here's our multiply?
   | Sum Expression Expression deriving (Eq, Show)

------------------------------------------------------------------------------
-- FIXME: unit tests, as usual
-- parsing
type Parser = Parsec Void String

term :: Parser Expression
term = Term <$> oneOf ['a'..'z']

expression' = expression <* eof

expression = (term <|> (between (char '(') (char ')') expression ))
             >>= next >>= summ >>= manyP

manyP :: Expression -> Parser Expression
manyP e = maybe e (const (Many e)) <$> optional (char '*')

summ :: Expression -> Parser Expression
summ e = maybe e (Sum e) <$> optional ( char '|' *> expression)

next:: Expression -> Parser Expression
next e = maybe e (Next e) <$> optional expression

------------------------------------------------------------------------------

-- all right, now automata

-- definition is stupid?
data State = State String [(Char, State)] deriving (Show, Eq)

finalState = State "final" []
errorState = State "error" []

-- epsilon?

buildAutomata :: State -> Expression -> State
buildAutomata s0 (Next e1 e2) = let
                                  s2 = buildAutomata s0 e2
                                in buildAutomata s2 e1
buildAutomata s0 (Term c) = State ['(',c,')'] [(c, s0)]

buildAutomata s0 (Many e) = undefined

buildAutomata s0 (Sum e1 e2) = undefined -- merge e1 and e2, how?



-- Sum Expression Expression deriving (Eq, Show)


main :: IO ()
main = someFunc
