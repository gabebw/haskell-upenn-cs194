module Main where

import Log
import LogAnalysis

main = testWhatWentWrong parse (whatWentWrongEnhanced "relish") "error.log"
