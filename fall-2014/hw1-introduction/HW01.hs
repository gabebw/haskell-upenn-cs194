module HW01 where

-- Exercise 1

lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2

toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = toDigits (dropLastDigit n) ++ [lastDigit n]

-- Exercise 3

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ns = reverse . doubleEveryOther' . reverse $ ns

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' (a:b:xs) = [a, b*2] ++ doubleEveryOther' xs
doubleEveryOther' (a:xs) = a:doubleEveryOther' xs

-- Exercise 4

sumDigits :: [Integer] -> Integer
sumDigits ns = sum $ concatMap toDigits ns

-- Exercise 5

validate :: Integer -> Bool
validate n = isMultipleOfTen $ sumDigits $ doubleEveryOther $ toDigits n
    where
        isMultipleOfTen n = n `mod` 10 == 0

-- Exercise 6

type Peg = String
type Move = (Peg, Peg)

-- Move n pegs from a to b using the third peg as temporary storage.
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b temp = hanoi (n-1) a temp b ++ [(a, b)] ++ hanoi (n-1) temp b a
