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
import Data.List

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

-- FIXME: priorities! aaaab|c should be parsed as (aaaab) | c
-- FIXME: a*b should be (a*)b
expression' = expression <* eof

expression = (term <|> (between (char '(') (char ')') expression ))
              >>= next >>= summ   >>= manyP

-- expression'' ???

manyP :: Expression -> Parser Expression
manyP e = maybe e (const (Many e)) <$> optional (char '*')

summ :: Expression -> Parser Expression
summ e = maybe e (Sum e) <$> optional ( char '|' *> expression)

next:: Expression -> Parser Expression
next e = maybe e (Next e) <$> optional expression

------------------------------------------------------------------------------

-- all right, now automata

-- definition is stupid? or not
data State = State (Maybe State) [(Char, State)] deriving Show

-- so automata, need to jump on next state,
-- which means I need to normalize states in case of |
-- so is (abc)*|(ab*)
-- equal (ab)*c*?

-- (abc)*|abd|abe
-- (abf)* | (abc)* | (ab) (d|e)
--
-- -a-> * -b-> * -f->
--             * -c->
--             * -d->
--             * -e->

finalState = State  Nothing []
errorState = State  Nothing []
emptyState = State  Nothing []

-- FIXME: runAutomata' "a((abcd)*)a" "aa"
stupdidParser x = maybe (error "can not parse") id  $ parseMaybe  expression' x

-- FIXME: runAutomata' "a((ac)*)a" "aa"
-- problem:                |     ^
--                         |_____| (* should backtrack on not matched, how?)

runAutomata :: State -> String -> Bool
runAutomata (State _ []) _ = True
runAutomata (State Nothing xs) "" = False
runAutomata (State (Just es) _) "" =
  let
    drillDown (State _ []) = True
    drillDown (State Nothing _) = False
    drillDown (State (Just s) _) = drillDown s
  in drillDown es
runAutomata (State es xs) (y:ys) =
  case find (\x -> fst x == y) xs of
    (Just (c,s)) -> runAutomata s ys
    Nothing -> maybe False (\es' -> runAutomata es' (y:ys)) es


-- runAutomata' "(ab)|b" "abaabab" yep, matched first two

runAutomata' e s = runAutomata (buildAutomata' Nothing finalState (stupdidParser e)) s


buildAutomata' :: Maybe State -> State -> Expression -> State
buildAutomata' es fs (Term c) = State es [(c, fs)]
buildAutomata' es fs (Next e1 e2) = let
                                  fs' = buildAutomata' es fs e2
                                  in buildAutomata' es fs' e1
buildAutomata' es fs (Many e) = let
                             fs' = buildAutomata' (Just fs) fs' e
                             in  fs'

buildAutomata' es fs (Sum e1 e2) = let
                                (State _ xs) = buildAutomata' es fs e1
                                (State _ ys) = buildAutomata' es fs e2
                             in (State es $ xs ++ ys)-- need epsilon? or eliminate




main :: IO ()
main = someFunc
