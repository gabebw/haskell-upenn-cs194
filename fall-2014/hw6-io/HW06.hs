{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HW06 where

import Data.Aeson
import Data.Monoid
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

----------------------------------------------------
-- Exercise 1: Convert (String "Y") -> (Bool True) and
-- (String "N") -> (Bool False)

ynToBool :: Value -> Value
ynToBool (Bool True) = String "Y"
ynToBool (Bool False) = String "N"
ynToBool v = v

ynToBoolWorks :: Bool
ynToBoolWorks =
    ynToBool (Bool True) == String "Y" &&
    ynToBool (Bool False) == String "N" &&
    ynToBool (Number 3) == Number 3

----------------------------------------------------
-- Exercise 2
-- Take in a ByteString containing JSON data and output either
-- an error message or a Value that has been processed by ynToBool.
parseData :: B.ByteString -> Either String Value
parseData = (fmap ynToBool) . eitherDecode

----------------------------------------------------
-- Exercise 3
-- Write a Market type and parser

data Market = Market { marketname :: T.Text
                     , x :: Float
                     , y :: Float
                     , state :: T.Text }
    deriving (Show, Generic)

instance FromJSON Market

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets = (fmap unwrapSuccessfulResult) . (fmap fromJSON) . parseData

unwrapSuccessfulResult :: Result a -> a
unwrapSuccessfulResult (Success a) = a

testParseMarkets :: IO ()
testParseMarkets = do
    fileData <- B.readFile "markets.json"
    let name = fmap marketname $ fmap (!! 0) $ parseMarkets fileData
    either print print name
