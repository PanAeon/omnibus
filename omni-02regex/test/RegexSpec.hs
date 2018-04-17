module RegexSpec( main, spec) where

import Lib
import Test.Hspec
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import Control.Monad(replicateM)


main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "regex" $ do
         describe "parse" $ do
           it "should parse simple literal" $
             stupdidParser "a" `shouldBe` (Term 'a')
           it "should parse simple concatenation" $
             stupdidParser "ab" `shouldBe` (Next (Term 'a') (Term 'b'))
           it "should parse simple choice" $
             stupdidParser "a|b" `shouldBe` (Sum (Term 'a') (Term 'b'))
           it "should parse simple parenthesis" $
             stupdidParser "(a)" `shouldBe` (Term 'a')
           it "should parse simple multiplication" $
             stupdidParser "a*" `shouldBe` (Many (Term 'a'))
           it "multiplication should bind to the closest literal" $
             stupdidParser "ab*" `shouldBe` (Next (Term 'a') (Many (Term 'b')))
           it "choice should have lowest priority" $
             stupdidParser "ab|cd" `shouldBe` (Sum (Next (Term 'a') (Term 'b')) (Next (Term 'c') (Term 'd')))
           it "everything should compose" $
             stupdidParser "(ab*|cd)*e" `shouldBe` (Next (Many (Sum (Next (Term 'a') (Many (Term 'b'))) (Next (Term 'c') (Term 'd')))) (Term 'e'))
             
         describe "automata" $ do
           it "should consume simple concatenation" $
             runS "ab" "ab" `shouldBe` (True, "")
