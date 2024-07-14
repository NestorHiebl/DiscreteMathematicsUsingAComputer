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

-- Exercise 9: Write a function that takes two lists of type [Maybe Int] and
-- examines the pair of list heads before looking at the rest of the lists. It
-- returns a list in which the Ints of each pair have been added if both are
-- of the form Just n, preserving any Just n value otherwise.

addJust :: [Maybe Int] -> [Maybe Int] -> [Maybe Int]
addJust l1 l2 = zipWith addJustHelper l1 l2
        where addJustHelper (Just a) (Just b) = (Just (a + b))
              addJustHelper Nothing (Just b) = Just b
              addJustHelper (Just a) Nothing = Just a
              addJustHelper Nothing Nothing = Nothing

-- Exercise 10: Define a data type that represents six different metals and
-- automatically creates versions of (==) and show.

data Metal = Aluminium |
             Silver |
             Titanium |
             Copper |
             Chrome |
             Platinum
             deriving (Eq, Show)

-- Exercise 11: Suppose that you have some coins that have been sorted into
-- piles, each of which contains only one kind of coin. Define a data type
-- that can be used to represent the piles of coins.

data CoinPile = Pennies Integer |
                Nickels Integer |
                Dimes Integer |
                Quarters Integer |
                HalfDollars Integer |
                Dollars Integer
                deriving (Eq, Show)

-- Exercise 12: A universal type is one in which any type can be represented.
-- Each different type is identified by its own constructor, which serves as a
-- distinguishing tag. For example, here is a universal type that represents
-- three different types of number:

-- data Number = INT Int | INTEGER Integer | FLOAT Float
--               deriving (Eq, Show)

-- Define a universal type that contains Booleans, characters and ints.

data SmallNum = BOOL Bool | CHAR Char | INT Int
                deriving (Eq, Show)

-- Exercise 13: Define a type that contains tuples of up to four elements.

data Tup a b c d = SINGLE (a) | DOUBLE (a,b) | TRIPLE (a,b,c) | QUAD (a,b,c,d)
           deriving (Show)

-- Exercise 14: The quadratic equation ax²+bx+c=0 has two roots, given by the
-- quadratic formula, as long as the discriminant is non-negative. If the
-- discriminant is negative the roots are complex. Define a function that finds
-- the real solutions of the quadratic equation, and reports failure if they
-- don't exist.

realRoots :: Double -> Double -> Double -> Maybe (Double, Double)
realRoots a b c = if det >= 0
                  then Just (((-b+det)/2*a),((-b-det)/2*a))
                  else Nothing
                  where det = sqrt ((b**2) - (4*a*c))

