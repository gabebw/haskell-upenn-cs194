{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.Char (isSpace)
import Data.List (sortBy)

-- Exercise 1

-- parseMessage "E 2 562 help help" == ValidLM (LogMessage (Error 2) 562 "help help")
-- parseMessage "I 29 la la la" == ValidLM (LogMessage Info 29 "la la la")
parseMessage :: String -> MaybeLogMessage
parseMessage ('E':' ':xs) = parseError xs
parseMessage ('I':' ':xs) = parseWarningOrInfo Info xs
parseMessage ('W':' ':xs) = parseWarningOrInfo Warning xs
parseMessage xs = InvalidLM xs

parseWarningOrInfo :: MessageType -> String -> MaybeLogMessage
parseWarningOrInfo messageType xs = ValidLM (LogMessage messageType (read $ leadingToken xs) (withoutLeadingToken xs))

parseError :: String -> MaybeLogMessage
parseError xs = ValidLM (LogMessage (Error (read $ leadingToken xs)) (read $ leadingToken $ withoutLeadingToken xs) (withoutLeadingToken $ withoutLeadingToken xs))

leadingToken :: String -> String
leadingToken = fst . break isSpace

withoutLeadingToken :: String -> String
withoutLeadingToken = (dropWhile isSpace) . snd . (break isSpace)

-- Exercise 2

validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly xs = map getLogMessage $ filter isValid xs
    where
        getLogMessage (ValidLM lm) = lm

isValid :: MaybeLogMessage -> Bool
isValid (InvalidLM _) = False
isValid _ = True

-- Exercise 3

parse :: String -> [LogMessage]
parse = validMessagesOnly . (map parseMessage) . lines

-- Exercise 4

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ ts1 _) (LogMessage _ ts2 _) = ts1 `compare` ts2

-- Exercise 5

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = sortBy compareMsgs

-- Exercise 6
-- Try:
-- > testWhatWentWrong parse whatWentWrong "error.log"
-- > testWhatWentWrong parse whatWentWrong "sample.log"

-- Takes an unsorted list of LogMessages, and returns a list of the messages
-- corresponding to any errors with a severity of 50 or greater, sorted by
-- timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = (map message) $ sortMessages $ (filter isSevereError) xs
    where
        isSevereError (LogMessage (Error severity) _ _) = severity > 50
        isSevereError _ = False
        message (LogMessage _ _ s) = s

