import Test.Hspec
import ExprT
import Lib

main :: IO ()
main = hspec $ do
    describe "eval" $ do
        it "correctly evaluates nested expressions" $ do
            eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

    describe "evalStr" $ do
        it "evaluates valid expressions" $ do
            evalStr "2+3*4" `shouldBe` Just 14
            evalStr "3+2" `shouldBe` Just 5
            evalStr "8*2" `shouldBe` Just 16

        it "returns Nothing for invalid expressions" $ do
            evalStr "2+3*" `shouldBe` Nothing
            evalStr "3+" `shouldBe` Nothing
            evalStr "*2" `shouldBe` Nothing

    describe "ExprT" $ do
        it "is an instance of Expr" $ do
            let old = Mul (Add (Lit 2) (Lit 3)) (Lit 4)
            let new = mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
            eval new `shouldBe` eval old

    describe "Integer" $ do
        it "is an instance of Expr" $ do
            let old = Mul (Add (Lit 2) (Lit 3)) (Lit 4)
            let new = mul (add (lit 2) (lit 3)) (lit 4) :: Integer
            eval new `shouldBe` eval old

    describe "Bool" $ do
        -- FIXME
        it "is an instance of Expr" $ do
            let new = eval (mul (add (lit 2) (lit 3)) (lit -1)) :: Bool
            eval new `shouldBe` eval old
