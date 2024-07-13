-- Exercise 3: Write a function that takes a character and returns True if the
-- character is 'a' and False otherwise.

is_a :: Char -> Bool
is_a 'a' = True
is_a _ = False

-- Exercise 4: Write a function that takes a string and returns True if the
-- string is "hello" and False otherwise.

is_hello :: [Char] -> Bool
is_hello "hello" = True
is_hello _ = False

-- Exercise 5: Write a function that takes a string and removes a leading
-- space if it exists.
-- Bonus: Remove all leading spaces from a string.

trim_space :: [Char] -> [Char]
trim_space (' ':xs) = trim_space xs
trim_space s = s

-- Exercise 6: Suppose a program has read in a list of numbers of type Int.
-- Each number is intended to represent a boolean value, where 0 means False,
-- 1 means True, and any other number constitutes invalid input. Write a
-- function convert :: [Int] -> [Bool] that converts a list of numbers to
-- corresponding booleans.

convert :: [Int] -> [Bool]
convert [] = []
convert (1:xs) = True : convert xs
convert (0:xs) = False : convert xs
convert (_:xs) = convert xs

-- Exercise 7: Write a function member0 :: String -> Bool that takes a list of
-- Char values and returns True if at least one of the characters is 0 and
-- False otherwise. Hint: use map and the function or, which takes a list of
-- boolean values and returns True if at least one of them is True.

member0 :: String -> Bool
member0 s = let helper c = if c == '0' then True else False
            in or (map helper s)

-- Exercise 8: Expand the following application:
-- foldr max 0 [1,5,3]

expanded = (max 1 (max 5 (max 3 0)))

