module HW05 where

import Ring
import Parser

import Data.Maybe (listToMaybe)

-- Exercise 2: Ring and Parsable instances for Mod5

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
