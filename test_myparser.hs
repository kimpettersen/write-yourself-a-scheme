import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import MyParser hiding (main)



main :: IO ()
main = hspec $ do
    describe "Basic Parsing" $ do
        it "String" $ do
           readExpr "\"potato\"" `shouldBe` "Found Value String \"potato\""
        it "Number" $ do
           readExpr "123" `shouldBe` "Found Value Number 123"
        it "Bool" $ do
           readExpr "#t" `shouldBe` "Found Value Bool True"
        it "Escaped quote" $ do
           readExpr "\"pot\\\"ato\"" `shouldBe` "Found Value String \"pot\\\"ato\""
        it "Escaped \\n" $ do
            readExpr "\"new\nline\"" `shouldBe` "Found Value String \"new\\nline\""
        it "Escaped \\r" $ do
            readExpr "\"eof\reof\"" `shouldBe` "Found Value String \"eof\\reof\""
        it "Escaped \\t" $ do
            readExpr "\"tab\ttab\"" `shouldBe` "Found Value String \"tab\\ttab\""
        it "Binary #o10" $ do
            readExpr "#o10" `shouldBe` "Found Value Number "
