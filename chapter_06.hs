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
                (And R Q)
            )
            (Imp (And P R) (And R Q))
        )
    (Imp Q (Imp (And P R) (And R Q)))

proof1_valid :: IO ()
proof1_valid = check_proof example_theorem proof1

proof2 :: Proof
proof2 =
    ImpI
        (ImpI
            (AndI
                (Assume Q,
                (AndER
                    (Assume (And P R))
                R))
            (And R Q))
        (Imp (And P R) (And R Q)))
    (Imp Q (Imp (And P R) (And R Q)))

proof2_valid :: IO ()
proof2_valid = check_proof example_theorem proof2

-- Exercise 22: Suppose we simply replace R ∧ Q below the {∧I} line with
-- Q ∧ R. This fixes the Invalid And-Introduction error, but it introduces
-- another error into the proof.
--  a) Edit proof2 to reflect this change; call the result proof3.
--  b) Decide exactly what is wrong with proof3.
--  c) Run the proof checker on proof3, and see whether it reports the same
--  error that you predicted.

proof3 :: Proof
proof3 =
    ImpI
        (ImpI
            (AndI
                (Assume Q,
                (AndER
                    (Assume (And P R))
                R))
            (And Q R))
        (Imp (And P R) (And R Q)))
    (Imp Q (Imp (And P R) (And R Q)))

-- The checker does not consider (And Q R) and (And R Q) to be equivalent. The
-- change would have to be made to all instanced to (And R Q) as well as the
-- theorem.

proof3_valid :: IO ()
proof3_valid = check_proof example_theorem proof3

-- Exercise 23: Define each of the following well-formed formulas as a Haskell
-- value of type Prop.
--  a) P
--  b) Q ∨ False
--  c) Q → (P → (P ∧ Q))
--  d) P ∧ (¬Q)
--  e) ¬P → Q
--  f) (P ∧ Q) ∨ (¬P ∧ Q) → (P ∧ Q)

e23a = P
e23b = (Or Q FALSE)
e23c = (Imp Q (Imp P (And P Q)))
e23d = (And P (Not Q))
e23e = (Imp (Not P) Q)
e23f =
    (Or
        (And P (Not Q))
        (Imp
            (And (Not P) Q)
            (Or P Q)
        )
    )

-- Exercise 34: Prove the following:
t34 :: Theorem
t34 = (Theorem [(And A (Not A))] FALSE)

p34 :: Proof
p34 = ImpE
    (
        (AndEL (Assume (And A (Not A))) A),
        (AndER (Assume (And A (Not A))) (Not A))
    )
    FALSE
p34_valid :: IO ()
p34_valid = check_proof t34 p34


-- Exercise 35: Prove the following:
t35 :: Theorem
t35 = Theorem [A] (Imp (Imp A FALSE) FALSE)

p35 :: Proof
p35 = ImpI
    (ImpE
        ((Assume A), (Assume (Imp A FALSE)))
        FALSE
    )
    (Imp (Imp A FALSE) FALSE)

p35_valid :: IO ()
p35_valid = check_proof t35 p35

-- Exercise 36: Prove the following:
t36 :: Theorem
t36 = Theorem [A, A `Imp` B, B `Imp` C, C `Imp` D] D

p36 :: Proof
p36 =
    ((
        (Assume A, Assume (A `Imp` B))
    {---------------------------------} `ImpE`
        B, Assume (B `Imp` C))
    {---------------------------------} `ImpE`
        C, Assume (C `Imp` D))
    {---------------------------------} `ImpE`
        D

p36_valid = check_proof t36 p36

