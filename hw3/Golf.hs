-- http://www.seas.upenn.edu/~cis194/spring13/hw/03-rec-poly.pdf
module Golf where

import Data.List (group, transpose, nubBy, intercalate, sortBy, sort)

type OccurrenceWithMax = (Int, Int, Int)
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
highestLineLength = foldl (\acc (_, n) -> max acc n) 0

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
line (value, count, lineLength) = (show value) ++ "=" ++ stars
    where
        stars = (take count $ repeat '*') ++ (take (lineLength - count) $ repeat ' ')
