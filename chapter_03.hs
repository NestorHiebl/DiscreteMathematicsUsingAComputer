import Numeric.Natural

-- Peano numbers are a great way to demonstrate the uses of recursion over
-- general algebraic types.
data Peano = Zero | Succ Peano deriving Show

-- Increment a Peano number
inc :: Peano -> Peano
inc a = Succ a

-- Decrement a Peano number
dec :: Peano -> Peano
dec Zero = Zero
dec (Succ a) = a

-- Add two Peano numbers
add :: Peano -> Peano -> Peano
add Zero b = b
add (Succ a) b = Succ (add a b)

-- Exercise 1: Write a recursive function copy that copies its list argument.

copy :: [a] -> [a]
copy [] = []
copy (x:xs) = x : copy xs

-- Exercise 2: Write a function inverse that takes a list of pairs and swaps
-- the pair elements.

inverse :: [(a,b)] -> [(b,a)]
inverse [] = []
inverse (x:xs) = (snd x, fst x) : inverse xs

-- Exercise 3: Write a function which takes two sorted lists and returns a
-- sorted list containing the elements of each.

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x < y then
                          x : merge xs (y:ys)
                      else
                          y : merge (x:xs) ys

-- Exercise 4: Write (!!!), a function that takes a natural number n and a list
-- and selects the nth element of the list. List elements are indexed from 0,
-- and since the type of the incoming number does not prevent it from being
-- out of range, the result should be a Maybe type. For example,
--  [1,2,3]!!!0 ==> Just 1
--  [1,2,3]!!!2 ==> Just 3
--  [1,2,3]!!!5 ==> Nothing

(!!!) :: [a] -> Natural -> Maybe a
(!!!) [] i = Nothing
(!!!) (x:xs) 0 = Just x
(!!!) (x:xs) i = xs!!!(i-1)

-- Exercise 5: Write a function lookup that takes a value and a list of pairs,
-- and returns the second element of the pair that has the value as its first
-- element. Use a Maybe type to indicate whether the lookup succeeded. For
-- example,
--  lookup 5 [(1,2),(5,3)] ==> Just 3
--  lookup 6 [(1,2),(5,3)] ==> Nothing 3

lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup e [] = Nothing
lookup e (x:xs) = if e == fst x then (Just (snd x)) else Main.lookup e xs

-- Exercise 6: Write a function that counts the number of times an element
-- appears in a list.

count :: Eq a => a -> [a] -> Int
count e [] = 0
count e (x:xs) = if e == x then 1 + (count e xs) else count e xs

-- Exercise 7: Write a function that takes a value e and a list of values xs
-- and removes all occurences of e from xs.

remove :: Eq a => a -> [a] -> [a]
remove e [] = []
remove e (x:xs) = if e == x then remove e xs else x : (remove e xs)

-- Exercise 8: Write a function that removes alternating elements of a list
-- argument, starting with the first one. For example,
--  deinterlace [1,2,3,4,5,6,7] ==> [2,4,6]

-- Exercise 9: Write a function extract :: [Maybe a] -> [a] that takes a list
-- of Maybe values and returns the elements they contain. For example,
--  extract [Just 3, Nothing, Just 7] -> [3, 7]
-- Note: This exercise has the same objective as exercise 20 from chapter 1,
-- with the difference that this implementation uses recursion instead of a
-- list comprehension.

-- Exercise 10: Write a function that takes two strings. If the second string
-- appears within the first, it returns the index identifying where it starts.
-- Indexes start from 0. For example,
--  strstr "abcde" "bc" ==> Just 1
--  strstr "abcde" "fg" ==> Nothing

