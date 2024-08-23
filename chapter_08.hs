import Stdm

-- Exercise 2: Work out the values of the following set expressions, and then
-- check your answer using the Haskell expression that follows.

e2a = [1,2,3] +++ [3]
e2b = [4,2] +++ [2,4]
e2c = [1,2,3] *** [3]
e2d = [] *** [1,3,5]
e2e = [1,2,3] ~~~ [3]
e2f = [2,3] ~~~ [1,2,3]
e2g = [1,2,3] *** [1,2]
e2h = [1,2,3] +++ [4,5,6]
e2i = ([4,3] ~~~ [5,4]) *** [1,2]
e2j = ([3,2,4] +++ [4,2]) ~~~ [2,3]
e2k = subset [3,4] [4,5,6]
e2l = subset [1,3] [4,1,3,6]
e2m = subset [] [1,2,3]
e2n = setEq [1,2] [2,1]
e2o = setEq [3,4,6] [2,3,5]
e2p = [1,2,3] ~~~ [1]
e2q = [] ~~~ [1,2]

-- Exercise 3: The function
--  powerset :: (Eq a, Show a) => Set a -> Set (Set a)
-- takes a set and returns its power set. Work out the values of the following
-- expressions:

e3a = powerset [3,2,4]
e3b = powerset [2]

