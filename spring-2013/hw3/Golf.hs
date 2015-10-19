-- http://www.seas.upenn.edu/~cis194/spring13/hw/03-rec-poly.pdf
module Golf where

import Data.List (group, transpose, nub, intercalate, sortBy, sort, zip)

--
-- Exercise 1: Hopscotch
-- Return a list of lists, where the nth list has _every_ nth item from the
-- original list.
skips :: [a] -> [[a]]
skips xs = map (skips' xs) [1..(length xs)]

-- Get every nth item from a list.
-- So `skips' 2 xs` finds every 2nd item.
-- It uses `elemsWithIndices` to create a list of tuples with each item tagged
-- with its index, like `[(1, "a"), (2, "b")]`.
-- Then it filters that list based on whether the index is divisible by n.
-- Then it grabs the snd item of each tuple.
skips' :: [a] -> Int -> [a]
skips' [] n = []
skips' xs n = map snd $ filter dividesInto (elemsWithIndices xs)
    where
        dividesInto (x, _) = x `mod` n == 0

elemsWithIndices :: [a] -> [(Int, a)]
elemsWithIndices = zip [1..]

--
-- Exercise 2: Local Maxima
--

-- A local maximum of a list is an element of the list which is strictly
-- greater than both the elements immediately before and after it.
--
-- > localMaxima [2,9,5,6,1] == [9,6]
-- > localMaxima [2,3,4,1,5] == [4]
-- > localMaxima [1,2,3,4,5] == []
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:[]) = if y > x && y > z then [y] else []
localMaxima (x:y:z:xs) = localMaxima [x, y, z] ++ localMaxima (y:z:xs)
localMaxima _ = []

--
-- Exercise 3: Histogram
--

-- A number, the number of occurrences of that number, and the most occurrences
-- across all numbers so we know how much to pad shorter lines.
-- Why call it Bucket? https://en.wikipedia.org/wiki/Data_binning
data Bucket = Bucket {
    num :: Int
    , occurrences :: Int
    , maxOccurrences :: Int
    } deriving (Show)

-- Always compare Bucket based on the number it represents
instance Eq Bucket where
    (Bucket n1 _ _) == (Bucket n2 _ _) = n1 == n2

instance Ord Bucket where
    (Bucket n1 _ _) `compare` (Bucket n2 _ _) = n1 `compare` n2

-- Given a list of numbers 0-9, returns a histogram of each number's frequency.
histogram :: [Int] -> String
histogram [] = ""
histogram ns = intercalate "\n" .
    transpose .
    map (reverse . line) .
    fillInMissingBuckets .
    buckets $ ns

-- Given a list of numbers like [1, 1, 1, 5], create a list of Buckets.
buckets :: [Int] -> [Bucket]
buckets ns = map makeBucket grouped
    where
        makeBucket ngroup@(n:_) = Bucket n (length ngroup) maximumSize
        maximumSize = maximum $ map length grouped
        grouped = group (sort ns)

-- A given list of buckets may have missing buckets for some of the numbers.
-- For example, if the original list is [1, 1, 2], it only has Buckets for 1 and 2.
-- This would add Buckets with a count of 0 for the numbers 3-10.
fillInMissingBuckets :: [Bucket] -> [Bucket]
fillInMissingBuckets bs@(b:_) = sort $ nub (bs ++ empty0to9)
    where
        empty0to9 = map (\n -> Bucket n 0 most) [0..9]
        most = maxOccurrences b

-- Given a Bucket, print out a _horizontal_ graph of the occurrences of that value.
-- It will be transformed into a vertical version by `transpose` in `histogram`.
line :: Bucket -> String
line (Bucket n count lineLength) = (show n) ++ "=" ++ stars ++ paddingSpaces
    where
        stars = replicate count '*'
        paddingSpaces = replicate (lineLength - count) ' '
