{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HW06 where

import Data.Aeson
import Data.Monoid
import GHC.Generics
import Data.List (sort)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

-- Exercise 1: ynToBool
-- Convert "Y" to True and "N" to False, inside aeson
ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Array a) = Array $ V.map ynToBool a
ynToBool (Object o) = Object $ HM.map ynToBool o
ynToBool v = v

-- Exercise 2: parseData
-- Take a ByteString of JSON and decode it, processing with ynToBool
parseData :: B.ByteString -> Either String Value
parseData = fmap ynToBool . eitherDecode

-- Exercise 3: A Market type, and parseMarkets
-- x is longitude
-- y is latitude
data Market = Market { marketname :: T.Text
                     , x :: Float
                     , y :: Float
                     , state :: T.Text }
    deriving (Show, Generic, Eq)

instance FromJSON Market

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets bs = case fmap fromJSON (parseData bs) of
                  (Left s) -> Left s
                  (Right (Error s)) -> Left s
                  (Right (Success a)) -> Right a

-- Exercise 4: loadData to load markets.json
loadData :: IO [Market]
loadData = fmap (unwrapMarkets . parseMarkets) fileContents
    where
        fileContents = B.readFile "markets.json"
        -- Pull out [Market], failing if we can't
        -- And yes, `either fail id` is gross
        unwrapMarkets :: Either String [Market] -> [Market]
        unwrapMarkets = either fail id

-- Exercise 5: Write OrdList and its Monoid instance
-- OrdList is a wrapper around a list (like Data.Monoid.First is a wrapper
-- around Maybe) that keeps the list ordered.

data OrdList a = OrdList { getOrdList :: [a] } deriving (Eq, Show)

instance (Ord a) => Monoid (OrdList a) where
    mappend (OrdList a) (OrdList b) = OrdList $ sort (a ++ b)
    mempty = OrdList []

-- Does the OrdList monoid work?
ordListWorks =
    (evens <> odds) == (OrdList [1..6]) &&
    (evens <> odds) == (odds <> evens) &&
    (evens <> mempty) == evens &&
    (odds <> mempty) == odds
    where
        combinedEvensOdds = evens <> odds
        combinedOddsEvens = evens <> odds
        evens = OrdList [2, 4, 6]
        odds = OrdList [1, 3, 5]

-- Exercise 6: write a search function given the searcher type

type Searcher m = T.Text -> [Market] -> m

search :: Monoid m => (Market -> m) -> Searcher m
search mk_m text = mconcat . map mk_m . filter matchesName
    where
        matchesName :: Market -> Bool
        matchesName = (text `T.isInfixOf`) . marketname

-- Try it with a List monoid:
-- markets <- loadData
-- search (:[]) "Wednesday" markets

-- Exercise 7: write firstFound, which finds the first result of
-- a search, if there is one
-- Monoid used: First

firstFound :: Searcher (Maybe Market)
firstFound text = getFirst . search (First . Just) text

-- Exercise 8: write lastFound, which finds the last result of a search, if
-- there is one
-- Monoid used: Last

lastFound :: Searcher (Maybe Market)
lastFound text = getLast . search (Last . Just) text

-- Exercise 9: find all markets in a search
-- Monoid used: list (the robot monkey)

allFound :: Searcher [Market]
allFound = search (:[])

-- Exercise 10: count the number of markets found by a search
-- Monoid used: Sum
-- Each Market is mapped to `Sum 1`

numberFound :: Searcher Int
numberFound t = getSum . search (\_ -> Sum 1) t

-- Exercise 11: orderedNtoS
-- Search for markets, then order the results northernmost to southernmost.

data MarketFromNorthToSouth = MarketFromNorthToSouth { market :: Market } deriving (Show, Eq)

-- y is the latitude.
-- 0 is the Equator, and 90 is the North Pole.
-- Therefore we sort from most-to-least y value.
instance Ord MarketFromNorthToSouth where
    (MarketFromNorthToSouth m1) `compare` (MarketFromNorthToSouth m2) = (y m2) `compare` (y m1)

orderedNtoS :: Searcher [Market]
orderedNtoS text = map market . getOrdList . search (\m -> OrdList [MarketFromNorthToSouth m]) text
