module HW4Spec (main, spec) where

import Test.Hspec
import HW4

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "fun1'" $ do
        it "works for []" $ do
            let xs = []
            fun1' xs `shouldBe` fun1 xs

        it "works for [1]" $ do
            let xs = [1]
            fun1' xs `shouldBe` fun1 xs

        it "works for lists of integers" $ do
            let xs = [5..10]
            fun1' xs `shouldBe` fun1 xs

    -- describe "fun2'" $ do
    --     it "works for 1" $ do
    --         let n = 1
    --         fun2' n `shouldBe` fun2 n
    --
    --     it "works for 2" $ do
    --         let n = 2
    --         fun2' n `shouldBe` fun2 n
    --
    --     it "works for 10" $ do
    --         let n = 10
    --         fun2' n `shouldBe` fun2 n

    describe "foldTree" $ do
        it "correctly builds the initial tree" $ do
            let result = foldTree [5] :: Tree Int
            result `shouldBe` Node 0 Leaf 5 Leaf

        it "correctly builds a slightly larger tree" $ do
            let result = foldTree [5, 3, 10] :: Tree Int
            result `shouldBe` Node 1
                (Node 0 Leaf 3 Leaf)
                5
                (Node 0 Leaf 10 Leaf)
