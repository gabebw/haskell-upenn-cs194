module Main where

import Log
import LogAnalysis

main = do
    testWhatWentWrong parse (whatWentWrongEnhanced "relish") "error.log"
