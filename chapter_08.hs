import Stdm
import Data.List

-- Exercise 2: Work out the values of the following set expressions, and then
-- check your answer using the Haskell expression that follows.

e2a = [1,2,3] +++ [3]
e2b = [4,2] +++ [2,4]
e2c = [1,2,3] *** [3]
e2d = [] *** [1,3,5]
e2e = [1,2,3] ~~~ [3]
e2f = [2,3] ~~~ [1,2,3]
e2g = [1,2,3] *** [1,2]
e2h = [1,2,3] +++ [4,5,6]
e2i = ([4,3] ~~~ [5,4]) *** [1,2]
e2j = ([3,2,4] +++ [4,2]) ~~~ [2,3]
e2k = subset [3,4] [4,5,6]
e2l = subset [1,3] [4,1,3,6]
e2m = subset [] [1,2,3]
e2n = setEq [1,2] [2,1]
e2o = setEq [3,4,6] [2,3,5]
e2p = [1,2,3] ~~~ [1]
e2q = [] ~~~ [1,2]

-- Exercise 3: The function
--  powerset :: (Eq a, Show a) => Set a -> Set (Set a)
-- takes a set and returns its power set. Work out the values of the following
-- expressions:

e3a = powerset [3,2,4]
e3b = powerset [2]

-- Exercise 4: The cross product of two sets A and B is defined as the
-- function
--  crossproduct :: (Eq a, Show a, Eq b, Show b) => Set a -> Set b -> Set (a,b)
-- Evaluate these expressions:

e4a = crossproduct [1,2,3] ['a','b']
e4b = crossproduct [1] ['a','b']

-- Exercise 7: Write and evaluate a list comprehension that expresses the set
-- { x | x ∈ {1,2,3,4,5} ∧ x < 0 }

e7 = normalizeSet [x | x <- [1..5], x < 0]

-- Exercise 8: Write and evaluate a list comprehension that expresses the set
-- { x + y | x ∈ {1,2,3} ∧ y ∈ {4,5} }

e8 = normalizeSet [x+y | x <- [1..3], y <- [4,5]]

-- Exercise 9: Write and evaluate a list comprehension that expresses the set
-- { x + y | x ∈ {1,2,3,4,5,6,7,8,9,10} ∧ x is even }

e9 = normalizeSet [x | x <- [1..10], x `mod` 2 == 0]

-- Exercise 14: The function "smaller" takes a value and a list of values and
-- returns True if the value is smaller than the fist element in the list.
-- Using this function, write a function that takes a set and returns its
-- powerset. Use foldr.

smaller :: Ord a => a -> [a] -> Bool
smaller _ [] = True
smaller a (x:_) = a < x

-- Input set must be sorted for inner function to work!
powerset' :: Ord a => Set a -> Set (Set a)
powerset' set = normalizeSet $ foldr inner [[]] $ sort set
    where inner x acc = [x:xs | xs <- acc, not (elem x xs) && smaller x xs] ++ acc

-- Exercise 16: Using a list comprehension, write a function that takes two
-- sets and returns true if the first is a subset of the other.

subset' :: Eq a => Set a -> Set a -> Bool
subset' a b = foldr (&&) True [elem x b' | x <- a']
    where
        a' = normalizeSet a
        b' = normalizeSet b

-- Exercise 17: What is wrong with this definition of diff, a function that
-- takes two sets and returns their difference?
--  diff :: Eq a => [a] -> [a] -> [a]
--  diff set1 set2 = [e | e <- set2, not (elem e set1)]

-- The arguments are in the wrong positions inside the comprehension. The
-- correct definition would be:
diff :: Eq a => [a] -> [a] -> [a]
diff set1 set2 = [e | e <- set1, not (elem e set2)]

-- Exercise 18: What is wrong with this definition of intersection, a function
-- that takes two sets and returns their intersection?
--  intersection :: Eq a => [a] -> [a] -> [a]
--  intersection set1 set2 = [e | e <- set1, e <- set2]

-- The syntax above repeats all elements of set2 |set1| times.
intersection :: Eq a => [a] -> [a] -> [a]
intersection set1 set2 = [e | e <- set1, elem e set2]

-- Exercise 19: Write a function using a list comprehension that takes two
-- sets and returns their union.

union' :: Eq a => Set a -> Set a -> Set a
union' a b = normalizeSet [e | e <- a ++ b]

