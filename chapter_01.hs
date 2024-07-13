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

