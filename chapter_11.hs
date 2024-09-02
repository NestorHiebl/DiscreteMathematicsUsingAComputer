import Stdm

-- The Ackermann function - a very interesting example of a total computable
-- function that is not primitive recursive. Its result grows rapidly as the
-- arguments increase, with x broadly specifying the operation type that is to
-- be applied to y. Passing three as the first argument rougly corresponds to
-- performing exponentiation using the second argument. Values of x above
-- three resolve to approximate representations of hyperoperations following
-- exponentiation. This is not the original version described by Wilhelm
-- Ackermann, which has three arguments.
ack :: Integer -> Integer -> Integer
ack 0 y = y + 1
ack x 0 = ack (x - 1) 1
ack x y = ack (x - 1) (ack x (y - 1))

-- Exercise 1: Decide whether the following functions are partial or total,
-- amd then run the tests of the computer:
e1a = isPartialFunction [1,2,3] [2,3] [(1,Value 2),(2,Value 3),(3,Undefined)]
e1b = isPartialFunction [1,2] [2,3] [(1,Value 2),(2,Value 3)]

-- Exercise 3: What is the value of mystery x where mystery is defined as:
mystery :: Int -> Int
mystery x = if mystery x == 2 then 1 else 3

-- mystery never terminates.

-- Exercise 4: What is the value of mystery2 x where mystery2 is defined as:
mystery2 :: Int -> Int
mystery2 x = if x == 20 then 2 + mystery2 x else 3

-- 3 for all values except 20, which is undefined.

-- Exercise 5: Work out the values of the following expressions, and then
-- check your result by evaluating them with the computer:
increment :: Int -> Int
increment n = n + 1
e5a = map (increment.increment.increment) [1,2,3]
e5b = map ((+2).(*2)) [1,2,3]

-- Exercise 12: Determine whether the functions in these examples are
-- injective, and check your conclusions using the computer:
e12a = isInjective [1,2,3] [2,4] [(1,Value 2),(2,Value 4),(3,Value 2)]
e12b = isInjective [1,2,3] [2,3,4] [(1,Value 2),(2,Value 4),(3,Undefined)]

-- Exercise 15: Determine whether the following functions are bijective, and
-- check your conclusions using the computer:
e15a = isBijective [1,2] [3,4] [(1,Value 3),(2,Value 4)]
e15b = isBijective [1,2] [3,4] [(1,Value 3),(2,Value 3)]

-- Exercise 17: Determine whether the following functions are permutations, and
-- check using the computer:
e17a = isPermutation [1,2,3] [1,2,3] [(1,Value 2),(2,Value 3),(3,Undefined)]
e17b = isPermutation [1,2,3] [1,2,3] [(1,Value 2),(2,Value 3),(3,Value 1)]

