import Stdm

increment :: Integer -> Integer
increment x = x + 1

s :: [Integer]
s = [0, increment (s !! 0), increment (s !! 1)]

mappend' :: (a -> [b]) -> [a] -> [b]
mappend' _ [] = []
mappend' f (x:xs) = f x ++ mappend' f xs

newBinaryWords :: [Integer] -> [[Integer]]
newBinaryWords xs = [0:xs, 1:xs]

binWords = [0] : [1] : (mappend' newBinaryWords binWords)

-- Exercise 11: Alter the definition of newBinaryWords and binWords so that
-- they produce all of the octal numbers. An octal number is one that contains
-- only the digits 0 to 7

newOctals :: [Integer] -> [[Integer]]
newOctals xs = [i:xs | i <- [0..7]]

octals = [[i]| i <- [0..7]] ++ (mappend' newOctals octals)

-- First attempt at inductively defining the set of integers:

build :: a -> (a -> a) -> Stdm.Set a
build a f = set
    where set = a : map f set

builds :: a -> (a -> [a]) -> Stdm.Set a
builds a f = set
    where set = a : mappend' f set

nextInteger1 :: Integer -> Integer
nextInteger1 x = -x

integers1 :: [Integer]
integers1 = build 0 nextInteger1

-- The previous attempt is only capable of generating the number zero. Second
-- attempt at inductively defining the set of integers:

nextIntegers2 :: Integer -> [Integer]
nextIntegers2 x = [x + 1, x - 1]

integers2 :: [Integer]
integers2 = builds 0 nextIntegers2

-- The previous attempt does not provide a unique way of generating each
-- integer, which is inelegant and extremely inefficient. Third attempt at
-- inductively defining the set of integers:

nextIntegers3 :: Integer -> [Integer]
nextIntegers3 x = [x + 1, -(x + 1)]

integers3 = builds 0 nextIntegers3

-- The third is closer, but still has the same drawbacks as the second.
-- integer, which is inelegant and extremely inefficient. Fourth attempt:

nextInteger4 :: Integer -> Integer
nextInteger4 x = if x < 0 then x - 1 else x + 1

integers4 = build 0 nextInteger4

-- The fourth attempt does not generate the desired set at all, because all
-- calls to nextInteger4 enter the same execution branch, causing only
-- positiove numbers to be output. Fifth attempt:

nextIntegers5 :: Integer -> [Integer]
nextIntegers5 x = if x >= 0
    then [x + 1, -(x + 1)]
    else []

integers5 = builds 0 nextIntegers5

