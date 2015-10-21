{-# OPTIONS_GHC -Wall #-}
module LogAnalysis2 where

import Log

parseMessage :: String -> LogMessage
-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
parseMessage ('I':xs) = LogMessage Info (read $ head $ words xs) (unwords $ tail $ words xs)
parseMessage ('W':xs) = LogMessage Warning (read $ head $ words xs) (unwords $ tail $ words xs)
parseMessage ('E':xs) = LogMessage (Error (read $ head $ words xs)) (read $ head $ tail $ words xs) (unwords $ tail $ tail $ words xs)
parseMessage xs = Unknown xs

