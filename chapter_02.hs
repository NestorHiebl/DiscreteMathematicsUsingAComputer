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

