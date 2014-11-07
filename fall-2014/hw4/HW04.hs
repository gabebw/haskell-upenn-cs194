{-# OPTIONS_GHC -Wall #-}

module HW04 where

-- Exercise 1
--
-- We don't know how to turn a into b, so the only way to be sure of getting a b
-- is to take in a value of type b and return that.
ex1 :: a -> b -> b
ex1 _ x = x
-- Fancy point-free style.
-- ex1 = flip const

-- Exercise 2
--
-- Below are the only two solutions that match the type.
ex2 :: a -> a -> a
ex2 x _ = x
-- This could also return y, since both x and y are of type a:
ex2' :: a -> a -> a
ex2' _ y = y

-- Exercise 3
--
-- The Int can be ignored, since we just need to return something of type a. So
-- this just returns the parameter that is of type a. This is the only possible
-- solution given the type.
ex3 :: Int -> a -> a
ex3 = flip const

-- Exercise 4
--
-- This returns an a, and takes two parameters of type a. So there are two
-- possible functions: one that returns the first parameter of type a, and one
-- that returns the second parameter of type a.
ex4 :: Bool -> a -> a -> a
ex4' :: Bool -> a -> a -> a

ex4 _ x y = x
ex4' _ x y = y

-- Exercise 5
--
-- There are four possible functions, since Bool is limited to either True or
-- False. We can return True, or False, or the given Bool, or the negation of
-- the given Bool.
ex5 :: Bool -> Bool
ex5' :: Bool -> Bool
ex5'' :: Bool -> Bool
ex5''' :: Bool -> Bool

ex5 _ = True
ex5' _ = False
ex5'' a = a
ex5''' a = not a

-- Exercise 6
--
-- This is impossible given the type. The function takes in a function (a -> a),
-- but there's no way to determine a value of type a based solely on the
-- function. Therefore this function is impossible.
ex6 :: (a -> a) -> a
ex6 = error "Impossible"

-- Exercise 7
-- There are two possibilities. We have a function that takes an a and returns
-- an a, and a value that's of type a. So we can return the value, or we can
-- return the function called on the value.
ex7 :: (a -> a) -> a -> a
ex7' :: (a -> a) -> a -> a
ex7'' :: (a -> a) -> a -> a

ex7 f x = f x
-- Point-free:
ex7' = ($)
ex7'' _  x = x

-- Exercise 8
--
-- All that we can do is return the value unchanged since we don't know what
-- kind of functions we can call on a. We could do something like
-- `ex8 (x:xs) = xs`, but that's not really a different function to the type
-- system.
ex8 :: [a] -> [a]
ex8 = id

-- Exercise 9
--
-- Hey look, it's `map`! We have to map over [a] using the provided function
-- because it's the only guaranteed way to get [b].
ex9 :: (a -> b) -> [a] -> [b]
ex9 = map

-- Exercise 10
--
-- This function can't guarantee that it will always return a. If the value
-- passed in is `Just a`, then it can pull out the a, but if Nothing is passed
-- in, it wouldn't be able to do anything.

ex10 :: Maybe a -> a
ex10 = error "Impossible"

-- Exercise 11
--
-- There are two possibilities: given x, return Just x, or return Nothing.
-- Nothing will still be of type `Maybe a` due to the type signature.

ex11 :: a -> Maybe a
ex11' :: a -> Maybe a

-- Point-free type constructor!
ex11 = Just
ex11' _ = Nothing

-- Exercise 12
--
-- There are two possibilities: return the type unchanged, or return Nothing.
--
-- We can't always return `Just a` because it's possible that Nothing was passed
-- in. We could use pattern matching, but that's not a different function.
ex12 :: Maybe a -> Maybe a
ex12' :: Maybe a -> Maybe a

ex12 = id
ex12' _ = Nothing
