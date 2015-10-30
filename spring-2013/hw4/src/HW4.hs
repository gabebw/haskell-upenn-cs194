module HW4 where

-- Exercise 1: Break fun1 and fun2 into versions without guards.

-- Original:
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (2 `subtract`) . filter even

-- -- Original:
-- fun2 :: Integer -> Integer
-- fun2 1 = 0
-- fun2 n
--     | even n = n + fun2 (n `div` 2)
--     | otherwise = fun2 (3 * n + 1)

-- Exercise 2: Folding with trees

-- Each Node knows the height at that node
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- Build a _balanced_ binary tree using `foldr`.
-- Values on the left should be less than the node, and values on the right
-- should be more than the node.
foldTree :: (Ord a, Show a) => [a] -> Tree a
foldTree = foldr insert Leaf

-- A node with two leaves has depth 0.
level :: Tree a -> Integer
level (Node n _ _ _) = n
level Leaf = -1

-- Insert an item into a balanced Tree, keeping it balanced
insert :: (Ord a, Show a) => a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x tree@(Node n left tx right)
    -- This won't keep it balanced -- we may need to change the root entirely.
    -- If x <= tx, we definitely need to insert it on the left, but WHERE on the
    -- left?
    -- Balanced b/c we can just insert on the left
    | x <= tx && level left <= level right  = Node (n+1) (insert x left) tx right
    | x <= tx && level left > level right = error (show tree) -- need to rotate
    | x > tx && level right <= level left = Node (n+1) left tx (insert x right)
    | x > tx && level right > level left = undefined -- need to rotate
