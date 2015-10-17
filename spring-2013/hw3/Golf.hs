-- http://www.seas.upenn.edu/~cis194/spring13/hw/03-rec-poly.pdf
module Golf where

import Data.List (group, transpose, nub, intercalate, sortBy, sort, zip)

--
-- Exercise 1: Hopscotch
-- Return a list of lists, where the nth list has _every_ nth item from the
-- original list.
skips :: [a] -> [[a]]
skips l = map (skips' l) [1..(length l)]

-- Get every nth item from a list.
-- So `skips' 2 l` finds every 2nd item.
-- It uses `elemsWithIndices` to create a list of tuples with each item tagged
-- with its index, like `[(1, "a"), (2, "b")]`.
-- Then it filters that list based on whether the index is divisible by n.
-- Then it grabs the snd item of each tuple.
skips' :: [a] -> Int -> [a]
skips' [] n = []
skips' l n = map snd $ filter dividesInto (elemsWithIndices l)
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
-- across all numbers
data OccurrenceWithMax = OccurrenceWithMax {
    num :: Int
    , occurrences :: Int
    , maxOccurrences :: Int
    } deriving (Show)

-- A number along with the number of occurrences of that number
type Occurrence = (Int, Int)

instance Eq OccurrenceWithMax where
    (OccurrenceWithMax n1 _ _) == (OccurrenceWithMax n2 _ _) = n1 == n2

instance Ord OccurrenceWithMax where
    (OccurrenceWithMax n1 _ _) `compare` (OccurrenceWithMax n2 _ _) = n1 `compare` n2

-- Given a list of numbers 0-9, returns a histogram of each number's frequency.
histogram :: [Int] -> String
histogram [] = ""
histogram ns = intercalate "\n" . transpose .  map (reverse . line) . fillInMissingOccurrences . counts $ ns

-- Given a list of numbers like [1, 1, 1, 5], returns a list of
-- (number, numberOfOccurrences)
counts :: [Int] -> [OccurrenceWithMax]
counts ns = (map mapper) (group (sort ns))
    where
        mapper ns'@(n:_) = OccurrenceWithMax n (length ns') maximumSize
        maximumSize = maximum $ map length grouped
        grouped = group $ sort ns

setLineLength :: [Occurrence] -> [OccurrenceWithMax]
setLineLength l = map setter l
    where
        setter (a, b) = OccurrenceWithMax a b len
        len = highestLineLength l

highestLineLength :: [Occurrence] -> Int
highestLineLength = maximum . map snd

-- Looking at the first item in the tuple, fill in missing values with (n, 0) so
-- that the return value has tuples where n goes from 0 to 9.
fillInMissingOccurrences :: [OccurrenceWithMax] -> [OccurrenceWithMax]
fillInMissingOccurrences os@(o:_) = sort $ nub (os ++ empty0to9)
    where
        empty0to9 = map (\n -> OccurrenceWithMax n 0 most) [0..10]
        most = maxOccurrences o

-- Given a value, a count of occurrences of that value, and the line length,
-- print out a horizontal graph of the occurrences of that value
line :: OccurrenceWithMax -> String
line (OccurrenceWithMax value count lineLength) = (show value) ++ "=" ++ stars ++ paddingSpaces
    where
        stars = replicate count '*'
        paddingSpaces = replicate (lineLength - count) ' '
