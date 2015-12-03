-- http://www.seas.upenn.edu/~cis194/spring13/hw/03-rec-poly.pdf
module Golf2 (skips, localMaxima, histogram) where

skips :: [a] -> [[a]]
skips xs = map (skips' xs) [1..(length xs)]

skips' :: [a] -> Int -> [a]
skips' xs n
    -- If length xs < n, splitAt n will happily split it anyway, so we have to
    -- check for that.
    | length xs < n = []
    | otherwise = last front : skips' back n
        where
            (front, back) = splitAt n xs

localMaxima = undefined
histogram = undefined
