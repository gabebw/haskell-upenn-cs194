module CreditCard where

toDigits :: Integer -> [Integer]
toDigits number
  | number <= 0 = []
  | number < 10 = [number]
  | otherwise = (toDigits digits) ++ [digit]
      where
        (digits, digit) = number `divMod` 10

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- Double every *other* number beginning from the right, so second-to-last,
-- fourth-to-last, etc.
--
-- doubleEveryOther [1, 2, 3] == [1, 4, 3]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther xs = doubleEveryOther allButLastTwo ++ [(secondToLast * 2), final]
    where
        allButLastTwo = init $ init xs
        secondToLast = last $ init xs
        final = last xs

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

dividesBy10 :: Integer -> Bool
dividesBy10 = (== 0) . (`mod` 10)

-- Could this number be a valid credit card number?
validate :: Integer -> Bool
validate = dividesBy10 . sumDigits . doubleEveryOther . toDigits
