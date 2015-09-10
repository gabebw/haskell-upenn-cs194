module HW01 where

import System.IO (readFile)
import Data.Char (isDigit)

-- Exercise 1

lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2

toRevDigits :: Integer -> [Integer]
toRevDigits n
    | n <= 0 = []
    | otherwise = lastDigit n : toRevDigits (dropLastDigit n)

-- Exercise 3

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ns = zipWith (*) (cycle [1,2]) ns

-- Pattern matching
doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' [a] = [a]
doubleEveryOther' (a:b:xs) = a : b*2 : doubleEveryOther' xs

-- Exercise 4

sumDigits :: [Integer] -> Integer
sumDigits ns = sum $ concatMap toRevDigits ns

-- Exercise 5

luhn :: Integer -> Bool
luhn = isMultipleOfTen . sumDigits . doubleEveryOther . toRevDigits
    where
        isMultipleOfTen n = n `mod` 10 == 0

-- Exercise 6

type Peg = String
type Move = (Peg, Peg)

-- Return the list of moves to be performed to move the stack of
-- n discs from the peg a to peg b.
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a

-- Personal bonus exercise:
-- 1) Read in a file of credit card numbers
-- 2) Print "True" if every CC is valid
-- 3) Print "False" if any of them is invalid

-- Clean non-digits from a CC number, so
-- "12-34" becomes "1234" and suitable
-- for passing to `luhn`
clean :: String -> String
clean [] = []
clean (n:ns)
    | isDigit n = n : clean ns
    | otherwise = clean ns

-- Is every line a valid credit card number?
allValid :: [String] -> Bool
allValid ls = all (luhn . read . clean) ls

main = print . allValid . lines =<< readFile "files/valid-tricky.txt"
