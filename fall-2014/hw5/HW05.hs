module HW05 where

import Ring
import Parser

import Data.Maybe (listToMaybe)

------------------------
-- Exercise 2: Ring and Parsable instances for Mod5
------------------------

-- The integers modulo 5 form a ring with 5 elements: R = {0, 1, 2, 3, 4}.
data Mod5 = MkMod Integer deriving (Show, Eq)

instance Ring Mod5 where
    addId = MkMod 0
    addInv (MkMod x) = MkMod (negate x)
    mulId = MkMod 1

    add (MkMod x) (MkMod y) = MkMod $ (x + y) `mod` 5
    mul (MkMod x) (MkMod y) = MkMod $ (x * y) `mod` 5

instance Parsable Mod5 where
    -- The `parse` on the right is from the Parsable instance for Integer.
    parse = fromParsedInteger . parse

fromParsedInteger :: Maybe (Integer, String) -> Maybe (Mod5, String)
fromParsedInteger Nothing = Nothing
fromParsedInteger (Just (n, s)) = Just ((MkMod n), s)

-- Was the Mod5 instance of Ring defined properly?
mod5RingWorks :: Bool
mod5RingWorks = (add (MkMod 5) (MkMod 3) == MkMod 3) &&
    (add (MkMod 3) addId == MkMod 3) &&
    (add (MkMod 3) (addInv (MkMod 3)) == MkMod 0) &&
    (mul (MkMod 3) (MkMod 3) == MkMod 4) &&
    (mul (MkMod 3) mulId == MkMod 3)

-- Was the Mod5 instance of Parsable defined properly?
mod5ParsingWorks :: Bool
mod5ParsingWorks = (parse "3" == Just (MkMod 3, "")) &&
    (parseRing "1 + 2 * 5" == Just (MkMod 1))

------------------------
-- Exercise 3: Ring and Parsable instances for Mat2x2
------------------------

-- A row in a matrix of size 2
data Row = Row Integer Integer deriving (Show, Eq)
-- A 2x2 matrix has 2 rows (with each row having 2 elements)
data Mat2x2 = Mat2x2 Row Row deriving (Show, Eq)

instance Ring Mat2x2 where
    addId = Mat2x2 (Row 0 0) (Row 0 0)
    addInv (Mat2x2 (Row x1 y1) (Row x2 y2)) = Mat2x2 (Row (negate x1) (negate y1)) (Row (negate x2) (negate y2))
    mulId = Mat2x2 (Row 1 0) (Row 0 1)
    add (Mat2x2 r1 r2) (Mat2x2 r3 r4) = Mat2x2 (addRow r1 r3) (addRow r2 r4)
    -- tl = top left, br = bottom right, etc
    -- [ tl1 tr1 ]   [ tl2 tr2 ]   [ (tl1 * tl2 + tr1 * bl2) (tl1 * tr2 + tr1 * br2) ]
    -- [ bl1 br1 ] x [ bl2 br2 ] = [ (bl1 * tl2 + br1 * bl2) (bl1 * tr2 + br1 * br2) ]
    mul m1 m2 = Mat2x2 (firstRowMultiplied m1 m2) (secondRowMultiplied m1 m2)

-- Parse a string like "[[1,2],[3,4]]" into Just (Mat2x2, String)
-- The key is that parseIntoLists uses the [[Integer]] instance of `reads`.
instance Parsable Mat2x2 where
    parse = fmap parseIntoRow . parseIntoLists

parseIntoRow :: ([[Integer]], String) -> (Mat2x2, String)
parseIntoRow ([[a, b], [c, d]], s) = (Mat2x2 (Row a b) (Row c d), s)

parseIntoLists :: String -> Maybe ([[Integer]], String)
parseIntoLists = listToMaybe . reads

-- Multiply two matrices and get the top row
firstRowMultiplied :: Mat2x2 -> Mat2x2 -> Row
firstRowMultiplied (Mat2x2 (Row tl1 tr1) (Row bl1 br1)) (Mat2x2 (Row tl2 tr2) (Row bl2 br2)) = Row (tl1 * tl2 + tr1 * bl2) (tl1 * tr2 + tr1 * br2)

-- Multiply two matrices and get the bottom row
secondRowMultiplied :: Mat2x2 -> Mat2x2 -> Row
secondRowMultiplied (Mat2x2 (Row tl1 tr1) (Row bl1 br1)) (Mat2x2 (Row tl2 tr2) (Row bl2 br2)) = Row (bl1 * tl2 + br1 * bl2) (bl1 * tr2 + br1 * br2)

addRow :: Row -> Row -> Row
addRow (Row x1 y1) (Row x2 y2) = Row (x1 + x2) (y1 + y2)

-- Was the Mat2x2 instance of Ring defined properly?
mat2x2RingWorks :: Bool
mat2x2RingWorks = (add mat1234 mat5678) == (Mat2x2 (Row 6 8) (Row 10 12)) &&
    (add mat1234 (addInv mat1234)) == Mat2x2 (Row 0 0) (Row 0 0) &&
    (add mat1234 addId) == mat1234 &&
    (mul mat1234 mulId) == mat1234 &&
    (mul mat1234 mat5678) == (Mat2x2 (Row 19 22) (Row 43 50))
    where
        mat1234 = Mat2x2 (Row 1 2) (Row 3 4)
        mat5678 = Mat2x2 (Row 5 6) (Row 7 8)

-- Was the Mat2x2 instance of Parsable defined properly?
mat2x2ParsingWorks :: Bool
mat2x2ParsingWorks = (parse "[[3,4],[5,6]]" == Just ((Mat2x2 (Row 3 4) (Row 5 6)), "")) &&
    (parseRing "[[1,2],[3,4]] + [[5,6],[7,8]]" == Just (Mat2x2 (Row 6 8) (Row 10 12))) &&
    (parseRing "[[1,2],[3,4]] * [[5,6],[7,8]]" == Just (Mat2x2 (Row 19 22) (Row 43 50)))

------------------------
-- Exercise 4: Boolean Rings
------------------------

instance Ring Bool where
    -- Additive identity: x || False == x for all x
    addId = False
    addInv = not
    -- Multiplicative identity: x && True == x for all x
    mulId = True
    add = (||)
    mul = (&&)

instance Parsable Bool where
    parse = listToMaybe . reads

boolRingWorks :: Bool
boolRingWorks = (add True False) == True &&
    (add False False) == False &&
    (add False (addInv False)) == True &&
    (add True (addInv True)) == True &&
    (add True addId) == True &&
    (add False addId) == False &&
    (mul True False) == False &&
    (mul True True) == True &&
    (mul False mulId) == False &&
    (mul True mulId) == True

boolParsingWorks :: Bool
boolParsingWorks = (parse "True" == Just (True, "")) &&
    (parse "False" == Just (False, "")) &&
    (parseRing "True + False" == Just True) &&
    (parseRing "True * True" == Just True) &&
    (parseRing "True * False" == Just False)

main :: IO ()
main = do
    let mod5works = mod5RingWorks && mod5ParsingWorks
    let mat2x2works = mat2x2RingWorks && mat2x2ParsingWorks
    let boolWorks = boolRingWorks && boolParsingWorks
    let result = mod5works && mat2x2works && boolWorks
    print result
