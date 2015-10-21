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
exactMatches c1 c2 = length $ filter match $ zip c1 c2
    where
        match (x, y) = x == y

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors cs = map countColor colors
    where
        countColor c = length $ filter (== c) cs

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c1 c2 = sum $ zipWith min (countColors c1) (countColors c2)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exact inexact
    where
        exact = exactMatches secret guess
        inexact = (matches secret guess) - exact

-- Exercise 4 -----------------------------------------

-- a Code is consistent with a Move if the Code could have been the secret that
-- generated that move.

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess exact inexact) secret = exact == exact' && inexact == inexact'
    where
        exact' = exactMatches secret guess
        inexact' = (matches secret guess) - exact

-- Exercise 5 -----------------------------------------

-- Filter a list of Codes to all Codes consistent with the Move

filterCodes :: Move -> [Code] -> [Code]
filterCodes m cs = filter (isConsistent m) cs

-- Exercise 6 -----------------------------------------

-- Generate all codes of length n

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = map (:[]) colors
allCodes n = concatMap withColors $ allCodes (n-1)
    where
        -- Given a code, build a list of the "next step" codes: the same code
        -- but with another Red, the same code but with another Green, etc.
        withColors :: Code -> [Code]
        withColors code = map (\color -> color:code) colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = findMoves secret allMoves
    where
        allMoves = map (getMove secret) $ allCodes $ length secret

-- Given a secret and all possible moves
-- 1) stop if we found a 100% exact match
-- 2) otherwise, add the current move onto the internal list of moves
findMoves :: Code -> [Move] -> [Move]
findMoves secret [] = []
findMoves secret (m@(Move _ exact _):ms)
    | exact == length secret = [m]
    | otherwise = m:findMoves secret ms

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined



