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

