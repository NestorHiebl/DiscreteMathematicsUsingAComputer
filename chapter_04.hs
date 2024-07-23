-- This file currently only contains a portion of the chapter 4 exercises,
-- namely those that ask for specific haskell functions. Exercises focusing on
-- proofs may be added at a later date.

-- Exercise 22: Assume there is a function max that delivers the larger of its
-- two arguments. Write a function maximum that given a non-empty sequence of
-- values whose sizes can be compared (that is, valuse from a type of class
-- Ord), delivers the largest value in the sequence.

maximum :: Ord a => [a] -> a
-- Non-exhaustive patterns, as x:xs cannot be empty according to the
-- specification.
maximum (x:xs) = foldr (max) x xs

-- Exercise 24: Write a function that, given a sequence containing non-empty
-- sequences, delivers the sequence made up of the first elements of each of
-- those non-empty sequences.

heads :: [[a]] -> [a]
heads xss = map (head) xss

