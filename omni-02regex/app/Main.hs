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

-- definition is stupid? or not
data State = State [(Char, State)] deriving (Show, Eq)

finalState = State  []
errorState = State  []
emptyState = State  []

-- epsilon?


buildAutomata' :: Expression -> State
buildAutomata' (Term c) = State [(c, emptyState)]
-- ? => 'a' => s0
-- ? => 'b' => s1
-- ? => 'a' => s0 => 'b' => s1
buildAutomata' (Next e1 e2) = let
                                (State xs) = buildAutomata' e1
                                s1 = buildAutomata' e2
                                f (c, s) = (c, s1)
                              in State ( f <$> xs) -- also wrong?

--
--  ? => 'a' => s0
--  (? => 'a' => s1 => s2 => ..cycle => sn )*
-- -----------------------
--         => ('b' => y) <- next
-- ? => (x => 'a' => x)
buildAutomata' (Many e) = let
                             (State xs) = buildAutomata' e

                          in  undefined -- ids?, yeah wrong

-- ? => 'a' => s0      => a
--                   ?       => s0
-- ? => 'b' => s0      => b

buildAutomata' (Sum e1 e2) = let
                                (State xs) = buildAutomata' e1
                                (State ys) = buildAutomata' e2
                             in (State $ xs ++ ys)-- need epsilon? or eliminate

addTransition :: State -> Char -> State -> State
addTransition = undefined -- yeah, very funny, I can't modify prev state

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
