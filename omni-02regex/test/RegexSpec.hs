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
           it "should consume simple concatenation" $
             runS "a|b" "a" `shouldBe` (True, "")
           it "should consume simple concatenation" $
             runS "a|b" "b" `shouldBe` (True, "")
           it "should not consume empty input under concatenation" $
             runS "a|b" "" `shouldBe` (False, "")
           it "should not consume wrong input under concatenation" $
             runS "a|b" "c" `shouldBe` (False, "c")
           it "should run a series of regex tests" $
             sequence_ [ runS r i `shouldBe` o | (r,i,o) <- [
               ("ab|a", "a", (True, ""))
             , ("ab|a", "ab", (True, ""))
             , ("a|b", "c", (False, "c"))
             , ("(ab)*ac", "ac", (True, ""))
             , ("(ab)*ac", "", (False, ""))
             , ("(a*)*", "", (True, ""))
             , ("(ab|ac|ae|af|a)a", "aa", (True, ""))
             , ("(ab|ac|ae|af|a)a", "afa", (True, ""))
             , ("(a*)*", "aaaaaaa", (True, ""))
             , ("(a*|b*)", "aaaaaaaaaa", (True, ""))
             , ("(a*|b*)", "bbbbbb", (True, ""))
             , ("(a|b)*" , "abbbbaaaababbb", (True, ""))
             , ("(ab(ac)*ae)*", "abacacacae", (True, ""))
             , ("(ab(ac)*ae)*", "", (True, ""))
             , ("(ab(ac)*ae)*", "abae", (True, ""))
             , ("(ab(ac)*ae)*", "abadadf", (False, "dadf"))
             , ("(a|b*)*", "abbbbbaaabaaaba", (True, ""))
             , ("(a*|b*)*", "abaaaabbbaabb", (True, "")) -- yeah, :'(
             , ("(a*d|b*e)*", "aaadbbbeaaadbeadde", (True, ""))
             , ("((a*)(xb*)c)*", "aaaaxbbbbcaaxbc", (True, ""))
             ]]
