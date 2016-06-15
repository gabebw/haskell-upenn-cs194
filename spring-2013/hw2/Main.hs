module Main where

import Log (testParse, testWhatWentWrong)
import qualified LogAnalysis as LA1
import qualified LogAnalysis2 as LA2

main :: IO ()
main = logAnalysis2

logAnalysis1 :: IO ()
logAnalysis1 = LA1.main

logAnalysis2 :: IO ()
logAnalysis2 = do
    errors <- testWhatWentWrong LA2.parse LA2.whatWentWrong "error.log"
    mapM_ putStrLn errors
