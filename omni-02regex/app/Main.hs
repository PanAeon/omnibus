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
             >>= manyP >>= next  >>= summ

{-

-- FIXME: EBNF, top-down parsing and monadic parsing,
-- for fuck sake, can't parse bloody ax|bc*|ed???
expr = expr' (| expr')?
expr' = term term?
term  = [a-z]|expr

-}


expr = summ''

expr' = (expr'' >>= next'' >>= manyP)
expr'' = term <|> (between (char '(') (char ')') expr )

summ'' :: Parser Expression
summ'' = do
             x  <- expr'
             xs <- many ( char '|' *> expr')
             pure $ foldl (Sum) x xs
         --    maybe e (Sum e) <$> optional ( char '|' *> expr')

next'':: Expression -> Parser Expression
next'' e = maybe e (Next e) <$> optional expr'

manyP :: Expression -> Parser Expression
manyP e = maybe e (const (Many e)) <$> optional (char '*')

summ :: Expression -> Parser Expression
summ e = maybe e (Sum e) <$> optional ( char '|' *> expression)

next:: Expression -> Parser Expression
next e = maybe e (Next e) <$> optional expression


expression'' = manyP' <|> expression'''

expression''' = next' <|> expression''''

expression'''' = summ' <|> expression'''''

expression''''' = (term <|> (between (char '(') (char ')') expression'' ))



manyP' :: Parser Expression
manyP' = do
           t <- expression''
           maybe t (const (Many t)) <$> optional (char '*')

summ' = do
          e <- expression''
          maybe e (Sum e) <$> optional ( char '|' *> expression'')

next' = do
         e <- expression''
         maybe e (Next e) <$> optional expression''



stupdidParser x = maybe (error "can not parse") id  $ parseMaybe  (expr <* eof) x

-----------------------------------------------------------------------------------------------------------------------
-- all right, now automata
-----------------------------------------------------------------------------------------------------------------------


data Ref = Ref Char State
         -- | RecRef Char State
         -- | BckRef Char State
         | EpsRef State


data State =   State Bool [Ref]

instance Show State where
  show = printAutomata

printAutomata :: State -> String
printAutomata (State _ []) = "▣"
printAutomata (State r xs) =
  if r
  then "{" ++ (concat . intersperse " ,") (printRef <$> xs) ++ "}"
  else "[" ++ (concat . intersperse " ,") (printRef <$> xs) ++ "]"

printRef :: Ref -> String
printRef (Ref c (State True _)) = '\'':c:'\'':"??<--"
printRef (Ref c s) = '\'':c:'\'':"->" ++ (printAutomata s)
-- printRef (RecRef c s) = '\'':c:'\'':"➡" ++ (printAutomata s)
-- printRef (BckRef c _) = '\'':c:'\'':"⬅" ++ "??"
printRef (EpsRef s) = "⟿" ++ (printAutomata s)
-- printRef (BckEpsRef s) = "⬅⟿" ++ "??"

-- Careful with this one: buildFromString "(ab|ac|ae|af|a)a"
buildFromString s = buildAutomata (State False []) (stupdidParser s)

-- can switch first param for ref..
buildAutomata :: State -> Expression -> State
buildAutomata fs (Term c)  = State False [Ref c fs]
buildAutomata fs (Next e1 e2) = let
                                  fs' = buildAutomata fs e2
                                  in buildAutomata fs' e1

buildAutomata fs (Sum e1 e2) = let
                                 s1 = buildAutomata fs e1
                                 s2 = buildAutomata fs e2
                               in mergeStates s1 s2
buildAutomata fs (Many e)    = let
                                 (State xs) = buildAutomata fs' e
                                 -- xs' = markAsRec <$> xs -- here epsilon is an error
                                 fs' = undefined-- fmap all next states to BckRef
                              in  fs'
-- for now only ref && epsref
mergeStates :: State -> State -> State
mergeStates (State []) (State []) = State []
mergeStates (State xs) (State []) = State $ xs ++ [EpsRef (State [])]
mergeStates (State []) (State ys) = State $ [EpsRef (State [])] ++ ys
mergeStates (State xs) (State ys) = State $ xs'' ++ common ++ ys'' ++ epsilons
  where
     isEpsilon (EpsRef _) = True
     isEpsilon _          = False
     epsilons = take 1 (filter isEpsilon (xs ++ ys))
     xs' = filter (not . isEpsilon) xs
     ys' = filter (not . isEpsilon) ys
     sameChar (Ref c1 _) (Ref c2 _) = c1 == c2
     common = [Ref c1 (mergeStates s1 s2) | (Ref c1 s1) <- xs', (Ref c2 s2) <- ys', c1 == c2 ]
     xs'' = deleteFirstsBy sameChar xs' ys' -- knowing that chars are unique
     ys'' = deleteFirstsBy sameChar ys' xs'


--
--
-- buildAutomata' :: Maybe State -> State -> Expression -> State
-- buildAutomata' es fs (Term c) = State es [(c, fs)]
-- buildAutomata' es fs (Next e1 e2) = let
--                                   fs' = buildAutomata' es fs e2
--                                   in buildAutomata' es fs' e1
-- buildAutomata' es fs (Many e) = let
--                              fs' = buildAutomata' (Just fs) fs' e
--                              in  fs'
--
-- buildAutomata' es fs (Sum e1 e2) = let
--                                 (State _ xs) = buildAutomata' es fs e1
--                                 (State _ ys) = buildAutomata' es fs e2
--                              in (State es $ xs ++ ys)-- need epsilon? or eliminate

-- runAutomata :: State -> String -> Bool
-- runAutomata (State _ []) _ = True
-- runAutomata (State Nothing xs) "" = False
-- runAutomata (State (Just es) _) "" =
--   let
--     drillDown (State _ []) = True
--     drillDown (State Nothing _) = False
--     drillDown (State (Just s) _) = drillDown s
--   in drillDown es
-- runAutomata (State es xs) (y:ys) =
--   case find (\x -> fst x == y) xs of
--     (Just (c,s)) -> runAutomata s ys
--     Nothing -> maybe False (\es' -> runAutomata es' (y:ys)) es


-- runAutomata' "(ab)|b" "abaabab" yep, matched first two

-- runAutomata' e s = runAutomata (buildAutomata' Nothing finalState (stupdidParser e)) s

main :: IO ()
main = someFunc
