import Numeric.Natural
import Data.Maybe
import Data.List

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

deinterlace :: [a] -> [a]
deinterlace [] = []
deinterlace (x:[]) = []
deinterlace (x:y:xs) = y : deinterlace xs

-- Exercise 9: Write a function extract :: [Maybe a] -> [a] that takes a list
-- of Maybe values and returns the elements they contain. For example,
--  extract [Just 3, Nothing, Just 7] -> [3, 7]
-- Note: This exercise has the same objective as exercise 20 from chapter 1,
-- with the difference that this implementation uses recursion instead of a
-- list comprehension.

extract :: [Maybe a] -> [a]
extract [] = []
extract (x:xs) = if isJust x then (fromJust x) : extract xs else extract xs

-- Exercise 10: Write a function that takes two strings. If the second string
-- appears within the first, it returns the index identifying where it starts.
-- Indexes start from 0. For example,
--  strstr "abcde" "bc" ==> Just 1
--  strstr "abcde" "fg" ==> Nothing

strstr :: [Char] -> [Char] -> Maybe Int
strstr [] needle = Nothing
strstr haystack needle =
    let
        prefixcounter _ [] _ = Nothing
        prefixcounter n (x:xs) needle =
            if isPrefixOf needle (x:xs) then
                Just n
            else
                prefixcounter (n+1) xs needle
    in prefixcounter 0 haystack needle

-- Exercise 11: Write foldrWith, a function that behaves like foldr except that
-- it takes a function of three arguments and two lists.

foldrWith :: (a -> b -> b -> b) -> b -> [a] -> [b] -> b
foldrWith f acc [] l2 = acc
foldrWith f acc l1 [] = acc
foldrWith f acc (x:l1) (y:l2) = f x y (foldrWith f acc l1 l2)

-- Exercise 12: Using foldr, write a function mappend such that
--  mappend f xs = concat (map f xs)

mappend :: (a -> [b]) -> [a] -> [b]
mappend f xs = foldr (++) [] xs'
    where
        xs' = map f xs

-- Exercise 13: Write removeDuplicates, a function that takes a list and
-- removes all of its duplicate elements.

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = 
    if elem x xs
        then removeDuplicates xs
        else x : (removeDuplicates xs)

-- Exercise 14: Write a recursive function that takes a value and a list of
-- values and returns True if the value is in the list and False otherwise.

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) = if e == x then True else elem' e xs

-- Exercise 15: Write a function that takes two lists, and returns a list of
-- values that appear in both lists. This is one way to implement the
-- intersection operation on sets; see Chapter 8.

-- Exercise 16: Write a function that takes two lists, and returns True if all
-- the elements of the first list also occur in the other. This is one way to
-- determine whether one set is a subset of another; see Chapter 8.

-- Exercise 17: Write a recursive function that determines whether a list is
-- sorted.

-- Exercise 18: Show that the definition of factorial using foldr always
-- produces the same result as the recursive definiton given in the previous
-- section.

-- Exercise 19: Using recursion, define last, a function that takes a list and
-- returns a Maybe type that is Nothing if the list is empty.

-- Exercise 20: Using recursion, write two functions that expect a string
-- containing a number that contains a decimal point (for example, 23.455).
-- The first function returns the whole part of the number (i.e., the part to
-- the left

