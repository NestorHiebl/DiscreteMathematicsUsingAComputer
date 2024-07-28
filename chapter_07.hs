import Stdm

-- Exercise 8: Define the predicate p x y to mean x = y + 1, and let the
-- universe be {1,2}. Calculate the value of each of the following
-- expressions:
--  a) ∀x.(∃y.p(x,y))
--  b) ∃x,y.p(x,y)
--  c) ∃x.(∀y.p(x,y))
--  d) ∀x,y.p(x,y)

universe :: [Int]
universe = [1,2]

ex8a :: Bool
ex8a = forall universe inner
    where inner y = exists universe (== (y + 1))

ex8b :: Bool
ex8b = exists universe inner
    where inner y = exists universe (== (y + 1))

ex8c :: Bool
ex8c = exists universe inner
    where inner y = forall universe (== (y + 1))

ex8d :: Bool
ex8d = forall universe inner
    where inner y = forall universe (== (y + 1))

