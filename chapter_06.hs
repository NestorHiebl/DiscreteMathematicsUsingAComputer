import Stdm

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

