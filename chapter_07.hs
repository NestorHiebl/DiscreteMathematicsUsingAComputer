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

-- Note the existence of example_theorem in Stdm.lhs:
--  example_theorem =
--     Theorem
--         []
--         (Imp Q (Imp (And P R) (And R Q)))
--
-- proof1 represents a proof of example_theorem:

proof1 :: Proof
proof1 =
    ImpI
        (ImpI
            (AndI
                ((AndER
                    (Assume (And P R))
                    R),
                Assume Q)
            (And R Q))
        (Imp (And P R) (And R Q)))
    (Imp Q (Imp (And P R) (And R Q)))

proof1_valid :: IO ()
proof1_valid = check_proof example_theorem proof1

-- Exercise 22: Suppose we simply replace R ∧ Q below the {∧I} line with
-- Q ∧ R. This fixes the Invalid And-Introduction error, but it introduces
-- another error into the proof.
--  a) Edit proof2 to reflect this change; call the result proof3.
--  b) Decide exactly what is wrong with proof3.
--  c) Run the proof checker on proof3, and see whether it reports the same
--  error that you predicted.

