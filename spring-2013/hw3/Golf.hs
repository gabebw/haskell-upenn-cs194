-- http://www.seas.upenn.edu/~cis194/spring13/hw/03-rec-poly.pdf
module Golf where

import Data.List (group, transpose, nubBy, intercalate, sortBy, sort, zip)

--
-- Exercise 1: Hopscotch
-- Return a list of lists, where the nth list has every nth item from the
-- original list.
--
skips :: [a] -> [[a]]
skips l = map (skips' l) [1..(length l)]

skips' :: [a] -> Int -> [a]
skips' [] n = []
skips' l n = map snd $ filter dividesInto (elemsWithIndices l)
    where
        dividesInto (x, _) = (x+1) `mod` n == 0

elemsWithIndices :: [a] -> [(Int, a)]
elemsWithIndices = zip [0..]

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
type OccurrenceWithMax = (Int, Int, Int)
-- A number along with the number of occurrences of that number
type Occurrence = (Int, Int)

-- Given a list of numbers 0-9, returns a histogram of each number's frequency.
histogram :: [Int] -> String
histogram = intercalate "\n" . transpose . map (reverse . line) . setLineLength . fillInMissingOccurrences . counts

-- Given a list of numbers like [1, 1, 1, 5], returns a list of
-- (number, numberOfOccurrences)
counts :: [Int] -> [Occurrence]
counts = (map mapper) . (group . sort)
    where
        mapper x = (head x, length x)

setLineLength :: [Occurrence] -> [OccurrenceWithMax]
setLineLength l = map setter l
    where
        setter (a, b) = (a, b, len)
        len = highestLineLength l

highestLineLength :: [Occurrence] -> Int
highestLineLength = maximum . map snd

-- Looking at the first item in the tuple, fill in missing values with (n, 0) so
-- that the return value has tuples where n goes from 0 to 9.
fillInMissingOccurrences :: [Occurrence] -> [Occurrence]
fillInMissingOccurrences l = sortBy sorter $ nubBy matchingFst (l ++ empty0to9)
    where
        sorter (x, _) (y, _) = compare x y
        empty0to9 = map (\x -> (x, 0)) (take 10 [0..])
        matchingFst x y = fst x == fst y

-- Given a value, a count of occurrences of that value, and the line length,
-- print out a horizontal graph of the occurrences of that value
line :: OccurrenceWithMax -> String
line (value, count, lineLength) = (show value) ++ "=" ++ stars ++ paddingSpaces
    where
        stars = replicate count '*'
        paddingSpaces = replicate (lineLength - count) ' '
