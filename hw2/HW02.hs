{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches (a:as) (g:gs)
    | a == g = 1 + rest
    | otherwise = rest
    where
        rest = exactMatches as gs
exactMatches _ _ = 0

exactMatches' :: Code -> Code -> Int
exactMatches' actual guess = length $ filter match $ zip actual guess
    where
        match = uncurry (==)

-- Exercise 2 -----------------------------------------

-- For each peg in `code`, count how many times it occurs in `colors`
countColors :: Code -> [Int]
countColors code = map counter colors
    where
        counter color = length $ filter (color ==) code

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches actual guess = sum $ zipWith min (countColors actual) (countColors guess)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove actual guess = uncurry (Move guess) $ counts actual guess

counts :: Code -> Code -> (Int, Int)
counts actual guess  = (exactMatchesCount, inexactMatchesCount)
    where
        inexactMatchesCount = totalMatchesCount - exactMatchesCount
        exactMatchesCount = exactMatches actual guess
        totalMatchesCount = matches actual guess

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess inexactCount exactCount) possibleSecret =
    counts guess possibleSecret == (inexactCount, exactCount)


-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = map wrapInList colors
    where
        wrapInList x = [x]
allCodes n = concatMap nextStep (allCodes (n-1))
    where
        nextStep code = map (\color -> color:code) colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = reverse (makeMove secret [firstMove])
    where
        firstMove = getMove secret firstGuess
        firstGuess = head allCodes6

makeMove :: Code -> [Move] -> [Move]
makeMove _ [] = []
makeMove secret movesSoFar@(mostRecentMove:_)
    | isTheMove secret mostRecentMove = movesSoFar
    | otherwise = makeMove secret (nextMove:movesSoFar)
    where
        nextMove = getMove secret (head consistentCodes)
        consistentCodes = filterCodes mostRecentMove allCodes6

-- Did we guess the secret?
isTheMove :: Code -> Move -> Bool
isTheMove secret (Move guess _ _ ) = secret == guess

allCodes6 :: [Code]
allCodes6 = allCodes 6

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
