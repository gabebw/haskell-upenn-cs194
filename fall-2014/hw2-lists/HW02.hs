{-
**Notes**

I think the `bestWords` function (at the bottom) is interesting. It takes a list
of strings like ["bat", "cat", "rat"] and maps them into a list of tuples with
their score using `mapWithScore` like so:

    > mapWithScore  ["cat", "rat", "bat"]
    [(5,["bat","cat"]),(3,["rat"])]

Then it sorts that based on the score, finds the highest valued-tuple, and grabs
the list of words in that tuple. Since it's sorted, those are the words with the
highest value.
-}

module HW02 where

import Words
import Data.Maybe (fromMaybe)
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:

-- Exercise 1
-- formableBy "fun" ['x','n','i','f','u','e','l'] == True
-- formableBy "haskell" ['k','l','e','h','a','y','s'] == False
formableBy :: String -> Hand -> Bool
formableBy [] _ = True -- An empty string can be formed by anything
formableBy _ [] = False -- If you don't have any letters, nothing can be formed
formableBy (x:xs) hand
    | x `elem` hand = formableBy xs (delete x hand)
    | otherwise = False

-- Exercise 2
-- wordsFrom "abcd" == ["ab","ad","ba","bad","cab","cad","dab"]
wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

-- Exercise 3
-- wordFitsTemplate "??r?" "cxeabcl" "care" == True
-- wordFitsTemplate "??r?" "cxewbcl" "care" == False
-- wordFitsTemplate "??r" "cxeabcl" "care" == False
-- wordFitsTemplate "??r?" "cxeabcl" "abel" == False
wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate (t:ts) h (w:ws)
    -- Handle "ar?" "are"
    | t == w = wordFitsTemplate ts h ws
    | t == '?' && w `elem` h = wordFitsTemplate ts (delete w h) ws
    | otherwise = False
-- We ran out of letters in the template and the word at the same time
wordFitsTemplate [] _ [] = True
-- The template has letters left over, or the word does. Either way, not a
-- match.
wordFitsTemplate _ _ _ = False

-- Exercise 4
-- wordsFittingTemplate "??r?" "cxeabcl" == ["acre","bare","carb","care","carl","earl"]
wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate t h = filter (wordFitsTemplate t h) allWords

-- Exercise 5
-- Find the value of a scrabble word
scrabbleValueWord :: String -> Int
scrabbleValueWord = sum . map scrabbleValue

-- Exercise 6
-- Find the words with the maximum point value
-- bestWords (wordsFittingTemplate "??r?" "cxeabcl") == ["carb"]
-- bestWords ["cat", "rat", "bat"] == ["bat","cat"]
bestWords :: [String] -> [String]
bestWords [] = []
bestWords ws = highestValuedWords $ mapWithScore ws

highestValuedWords :: [(Int, [String])] -> [String]
highestValuedWords = snd . last . sortBy comparator
    where
        comparator (score1, _) (score2, _) = score1 `compare` score2

-- Turn a list of Scrabble words into tuples of `(score, [words,with,that,score])`.
mapWithScore :: [String] -> [(Int, [String])]
mapWithScore ws = foldl addToTally [(fst first, [snd first])] (tail vw)
    where
        first = head vw
        vw = valueAndWord ws

valueAndWord :: [String] -> [(Int, String)]
valueAndWord = map (\word -> (scrabbleValueWord word, word))

-- Used for folding a list of Strings into a list of tuples of
-- (score, [all, words, with, that, score])
addToTally :: [(Int, [String])] -> (Int, String) -> [(Int, [String])]
addToTally acc (score, w) = itemWithNew:(filter hasDifferentScore acc)
    where
        hasDifferentScore (a, _) = a /= score
        itemWithNew = (score, w:item)
        item = fromMaybe [] $ lookup score acc

-- Exercise 7
-- 'D' = double-letter square
-- 'T' = triple-letter square
-- '2' = double-word square
-- '3' = triple-word square
-- (Assume that the word matches the template)
--
-- scrabbleValueTemplate "?e??3" "peace" == 27
-- scrabbleValueTemplate "De?2?" "peace" == 24
-- scrabbleValueTemplate "??Tce" "peace" == 11
scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate stemplate word = wordMultiplier * baseScore
    where
        wordMultiplier = product $ map fst multiplierAndScore
        baseScore = sum $ map snd multiplierAndScore
        multiplierAndScore = map letterValueWithMultiplier $ zip stemplate word

-- Turn a tuple of (letter, multiplierChar) like ('p', 'D') into a tuple of
-- (wordMultiplier, letterValue). For example, double letter score ('D') has a
-- word multiplier of 1, since it doesn't change the overall word, but has a
-- doubled letterValue: (1, 2). Triple WORD score, '3', has a word multiplier of
-- 3 but an unchanged word value, like (3, 9).
letterValueWithMultiplier :: (Char, Char) -> (Int, Int)
letterValueWithMultiplier ('D', c) = (1, 2 * scrabbleValue c)
letterValueWithMultiplier ('T', c) = (1, 3 * scrabbleValue c)
letterValueWithMultiplier ('2', c) = (2, scrabbleValue c)
letterValueWithMultiplier ('3', c) = (3, scrabbleValue c)
letterValueWithMultiplier ('?', c) = (1, scrabbleValue c)
-- Assume that the character has been zipped with itself, like ('e', 'e')
letterValueWithMultiplier (_, c) = (1, scrabbleValue c)
