{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HW06 where

import Data.Aeson
import Data.Monoid
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.List (sort)
import Data.Maybe (listToMaybe)

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

----------------------------------------------------
-- Exercise 4

loadData :: IO [Market]
loadData = do
    fileData <- B.readFile "markets.json"
    -- If there's an error, fail with its message, otherwise return the parsed
    -- data
    either fail return $ parseMarkets fileData

----------------------------------------------------
-- Exercise 5

data OrdList a = OrdList { getOrdList :: [a] } deriving (Eq, Show)

instance (Ord a) => Monoid (OrdList a) where
    mappend x xs = OrdList $ sort $ (getOrdList x) ++ (getOrdList xs)
    mempty = OrdList []

evens :: OrdList Integer
evens = OrdList [2,4,6]

odds :: OrdList Integer
odds = OrdList [1,3,5]

combined :: OrdList Integer
combined = evens <> odds

ordListWorks :: Bool
ordListWorks = combined == OrdList [1,2,3,4,5,6]

----------------------------------------------------
-- Exercise 6

type Searcher m = T.Text -> [Market] -> m

-- Search for markets with the given text in their name. Convert each found
-- market into the given Monoid m and then combine all the results with
-- `mappend` and `mconcat`.
--
-- Example usage, for Monoid [a]:
--    search (\m -> [m]) (T.pack "ell") _markets_
search :: Monoid m => (Market -> m) -> Searcher m
search f text = mconcat . map f . (findMarketsNamedLike text)

findMarketsNamedLike :: T.Text -> [Market] -> [Market]
findMarketsNamedLike text = filter ((text `T.isInfixOf`) . marketname)

----------------------------------------------------
-- Exercise 7

-- Return the first market found by a search, if any are found at all.
firstFound :: Searcher (Maybe Market)
firstFound text = getFirst . search (First . Just) text
