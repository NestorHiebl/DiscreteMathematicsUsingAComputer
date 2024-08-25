import Stdm

increment :: Integer -> Integer
increment x = x + 1

s :: [Integer]
s = [0, increment (s !! 0), increment (s !! 1)]

-- Exercise 1: Is the following a chain? You can test your conclusion by
-- evaluating s in each case.
imp1 :: Integer -> Integer
imp1 1 = 2
imp1 x = error "imp1: premise does not apply"

imp2 :: Integer -> Integer
imp2 2 = 3
imp2 x = error "imp2: premise does not apply"

imp3 :: Integer -> Integer
imp3 3 = 4
imp3 x = error "imp3: premise does not apply"

s1 :: [Integer]
s1 = [1, imp1 (s1 !! 0), imp2 (s1 !! 1), imp3 (s1 !! 2)]

-- Exercise 6: Using the following definitons, determine whether 4 is in set
-- s6, given 1 ∈ s6 and the induction rule x ∈ s6 -> x + 2 ∈ s6.

fun6 :: Integer -> Integer
fun6 x = x + 2

s6 :: [Integer]
s6 = 1 : map fun6 s6

-- Exercise 9: Here is a Haskell equation that defines the set s9 inductively.
-- is 82 an element of s?

s9 :: [Integer]
s9 = 0 : map (+2) s9

-- Exercise 10: What set is defined by the following?

s10 :: [Integer]
s10 = 1 : map (*3) s10

-- Maps each element of a list to a new list, concatenating the results.
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

-- Exercise 17: Does ints, using the following definition, enumerate the
-- integers? If it does, then you should be able to pick any integer and see
-- it eventually in the output produced by ints. Will you ever see the
-- value -1?

nats :: [Integer]
nats = build 0 (1+)

negs :: [Integer]
negs = build (-1) (1-)

ints :: [Integer]
ints = nats ++ negs

-- No - appending a list to an endless list has no effect, because the final
-- element of the first list will never be generated. Negative one is not
-- reachable in ints.

-- Exercise 18: Does twos enumerate the set of even natural numbers?
twos :: [Integer]
twos = build 0 (*2)

-- No.

-- Exercise 19: What is wrong with the following definition of the stream of
-- natural numbers?
natsWrong = map (+1) nats ++ [0]

-- Because 0 is appended to the end of the list, natsWrong has no initial
-- element.

-- Exercise 20: What is the problem with the following definition of the
-- naturals?
naturals :: [Integer] -> [Integer]
naturals (i:acc) = naturals (i + 1:i:acc)

nats20 :: [Integer]
nats20 = naturals [0]

-- The recursion in naturals never terminates, making it impossible to
-- generate even a single element of nats20 after 0.

-- Exercise 21: Can we write a function that will take a stream of the naturals
-- (appearing in any order) and give the index of a particular number?

-- Unfortunately no. Depending on how the stream is generated, there may be an
-- infinite number of elements before the one passed to the function, making
-- it unreachable in finite time.

-- Exercise 28: Using data recursion, define the set of strings containing the
-- letter 'z'.

-- Doesn't really generate *all* strings containing 'z', but you get the idea.
newZStrings :: String -> [String]
newZStrings s = [c:s | c <- ['a'..'z']]

stringsWithZ :: [String]
stringsWithZ = "z" : (mappend' newZStrings stringsWithZ)

