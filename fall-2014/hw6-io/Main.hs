module Main where

import HW06
import qualified Data.ByteString.Lazy.Char8 as B

main = do
    fileData <- B.readFile "markets.json"
    print $ fmap (take 30) $ parseMarkets fileData
