import Data.Maybe

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

-- Exercise 15: Define a function

-- showMaybe :: Show a => Maybe a -> String

-- That takes a Maybe value and prints it.

showMaybe :: Show a => Maybe a -> String
showMaybe (Just a) = show a
showMaybe Nothing = show "Nothing"

-- Exercise 16: A Bit is an integer that is either 0 or 1. A Word is a list of
-- bits that represents a binary number. Here are some binary values that can
-- be represented by Words:
--  [1,0] => 2
--  [1,0,0,1] => 9
--  [1,1,1] => 7
-- We can define functions that are the Bit equivalent of or and and as
-- follows:

--  bitOr :: Int -> Int -> Int
--  bitOr 0 0 = 0
--  bitOr x y = 1

--  bitAnd :: Int -> Int -> Int
--  bitAnd 1 1 = 1
--  bitAnd x y = 0

-- Now it is possible to take the 'bitwise' and of two words as follows:

--  bitwiseAnd [1,0,0] [1,0,1]
--  => [bitAnd 1 1, bitAnd 0 0, bitand 0 1]
--  => [1,0,0]

-- Write a funciton bitwiseAnd that takes two Words and creates a third Word
-- that is the bitwise and of the two Words.

bitwiseAnd :: [Int] -> [Int] -> [Int]
bitwiseAnd w1 w2 = zipWith bitAnd w1 w2
                   where bitAnd 1 1 = 1
                         bitAnd x y = 0

-- Exercise 17: Each of the following expressions has a type error. Change the
-- expression so that the type error no longer occurs.
--  [1, False] => [1, 0] or [True, False]
--  [(3, True), (False, 9)] => [(True, 3), (False, 9)]
--  'a' > "b" => 'a' > 'b'
--  '2' ++ 'a' => "2" ++ "a"
--  2 == False => 2 == 3 or True == False
--  [[1], [2], [[3]]] => [[1], [2], [3]]

-- Exercise 18: What caused the type error in this definition and application?
--  f :: Num a => (a,a) -> a
--  f (x,y) = x + y
--  f (True, 4) => Both x and y need to be the same Num type

-- Exercise 19: Why does this definition produce an error when used?
--  f :: Maybe a -> [a]
--  f Nothing = []
--  f (Just 3) => No case defined for an instance of Just

-- Exercise 20: Write a list comprehension that takes a list of Maybe values
-- and returns a list of the Just constructor arguments.

extractJust :: [Maybe a] -> [a]
extractJust xs = [fromJust x | x <- xs, isJust x]

-- Exercise 21: Using a list comprehension, write a function that takes a list
-- of Int values and an Int value n and returns those elements in the list
-- that are greater than n.

greaterElems :: [Int] -> Int -> [Int]
greaterElems xs n = [x | x <- xs, x > n]

-- Exercise 22: Write a function that takes a list of Int values and an Int and
-- returns a list of indexes at which that Int appears.

getIndexes :: [Int] -> Int -> [Int]
getIndexes xs n = [x | x <- [0..(length xs) - 1], (xs!!x) == n]

-- Exercise 23: Write a list comprehension that produces a list giving all of
-- the positive integers that are not squares, in the range 1 to 20.

notSquares = [x | x <- [1..20], not (elem x [y*y | y <- [1..5]])]

-- Exercise 24: Write a function that uses foldr to count the number of times
-- a letter occurs in a string.

letterCount :: [Char] -> Char -> Int
letterCount xs c = let helper ch acc = if ch == c then acc + 1 else acc
                   in foldr helper 0 xs

-- Exercise 25: Write a function using foldr that takes a list and removes
-- each instance of a given letter.

letterRemover :: [Char] -> Char -> [Char]
letterRemover xs c = let helper ch acc = if ch == c then acc else ch:acc
                     in foldr helper "" xs

-- Exercise 26: Using foldr, write a function that reverses its list argument.

reverser :: [a] -> [a]
reverser xs = let helper e acc = acc ++ [e]
              in foldr helper [] xs

-- Exercise 27: Using foldl, write a function that takes a list and returns
-- the last element if there is one, otherwise it returns Nothing.

maybeLast :: [a] -> Maybe a
maybeLast xs = let helper acc e = Just e
               in foldl helper Nothing xs 

