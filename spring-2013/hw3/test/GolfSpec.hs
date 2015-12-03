module GolfSpec (main, spec) where

import Test.Hspec
import Golf2
import Data.List (intercalate)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "skips" $ do
        it "works on empty lists" $ do
            let result = [] :: [[Int]]
            skips [] `shouldBe` result

        it "works on [1]" $ do
            let expected = [[1]] :: [[Int]]
            skips [1] `shouldBe` expected

        it "works on longer lists" $ do
            skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
            skips [True, False] `shouldBe` [[True, False], [False]]

    describe "localMaxima" $ do
        it "finds local maxima" $ do
            localMaxima [2, 9, 5, 6, 1] `shouldBe` [9, 6]

        it "works on empty lists" $ do
            localMaxima [] `shouldBe` []

        it "returns an empty list when there are no maxima" $ do
            localMaxima [1, 2, 3, 4, 5] `shouldBe` []

    describe "histogram" $ do
        it "returns an empty string for an empty list" $ do
            histogram [] `shouldBe` ""

        it "returns the correct graph" $ do
            let result = intercalate "\n"
                         ["         *"
                         ," * *     *"
                         ,"=========="
                         ,"0123456789\n"]

            histogram [9, 1, 3, 9] `shouldBe` result
