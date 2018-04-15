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
type Parser = Parsec Void String

term :: Parser Expression
term = Term <$> oneOf ['a'..'z']

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

next'':: Expression -> Parser Expression
next'' e = maybe e (Next e) <$> optional expr'

manyP :: Expression -> Parser Expression
manyP e = (maybe e (const (Many e)) <$> optional (char '*')) >>= next''



stupdidParser x = maybe (error "can not parse") id  $ parseMaybe  (expr <* eof) x

-----------------------------------------------------------------------------------------------------------------------
-- all right, now automata
-----------------------------------------------------------------------------------------------------------------------


data Ref = Ref Char State
        | BckRef Char State

-- final, refs
data State =   State Bool [Ref]

instance Show State where
  show = printAutomata

printAutomata :: State -> String
printAutomata (State isFinal xs) =
  (if isFinal then "▣" else "") ++
   "[" ++ (concat . intersperse " ,") (printRef <$> xs) ++ "]"

printRef :: Ref -> String
printRef (Ref c s) = '\'':c:'\'':"->" ++ (printAutomata s)
printRef (BckRef c _) = '\'':c:'\'':"⬅" ++ "??"

-- Careful with this one: buildFromString "(ab|ac|ae|af|a)a"
buildFromString s = buildAutomata (State True [], False) (stupdidParser s)

-- can switch first param for ref..
buildAutomata :: (State,Bool) -> Expression -> State
buildAutomata (fs, isBck) (Term c)  = State False [if isBck then BckRef c fs else Ref c fs]
buildAutomata fs (Next e1 e2) = let
                                  fs' = buildAutomata fs e2
                                  in buildAutomata (fs',False) e1

buildAutomata fs (Sum e1 e2) = let
                                 s1 = buildAutomata fs e1
                                 s2 = buildAutomata fs e2
                               in mergeStates s1 s2
buildAutomata (fs@(State isFinal _), bs) (Many e)    = let -- bs here is lost, which is much worse :(
                                 (State _ xs) = buildAutomata (fs', True) e
                                 fs' = mergeStates  (State isFinal xs) fs
                              in fs'--- wow this is eureka moment, fuck
                                    -- FIXME: runS "(a*)*" "cabc", unit tests
-- for now only ref && epsref
mergeStates :: State -> State -> State
mergeStates (State _ []) (State _ []) = State True []
mergeStates (State _ xs) (State _ []) = State True xs  -- TODO: maybe unnecessary? how?
mergeStates (State _ []) (State r ys) = State True ys
mergeStates (State fs1 xs) (State fs2 ys) = State (fs1 || fs2) $ xs'' ++ bxs ++ common ++ ys'' ++ bys
  where
     isBck (BckRef _ _) = True
     isBck _          = False
     bxs = filter isBck xs
     bys = filter isBck ys
     xs' = filter (not . isBck) xs
     ys' = filter (not . isBck) ys
     sameChar (Ref c1 _) (Ref c2 _) = c1 == c2
     common = [Ref c1 (mergeStates s1 s2) | (Ref c1 s1) <- xs', (Ref c2 s2) <- ys', c1 == c2 ]
     xs'' = deleteFirstsBy sameChar xs' ys' -- knowing that chars are unique
     ys'' = deleteFirstsBy sameChar ys' xs'


runAutomata :: State -> String -> (Bool, String)  -- let it return unmatched
runAutomata (State True _) "" = (True, "")
runAutomata (State False _) "" = (False, "")
runAutomata (State isF refs) (y:ys) =
     case find (\x -> getC x == y) refs of
       (Just (Ref _ s)) -> runAutomata s ys
       (Just (BckRef _ s)) -> runAutomata s ys
       Nothing -> (False, y:ys)
   where
     getC (Ref c _) = c
     getC (BckRef c _) = c


runS e s = runAutomata (buildAutomata (State True [], False) (stupdidParser e)) s



main :: IO ()
main = someFunc
