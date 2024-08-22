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

-- Exercise 45: Suppose you were given the following code:
logicExpr1 :: Bool -> Bool -> Bool
logicExpr1 a b = a /\ b \/ a <=> a

logicExpr2 :: Bool -> Bool -> Bool
logicExpr2 a b = (a \/ b) /\ b <=> a /\ b
-- Each of these functions specifies a boolean expression. What are the truth
-- values of these expressions? How would you write a list comprehension that
-- can calculate the values for you to check your work?

logicExpr1Table = [logicExpr1 x y | x <- [True, False], y <- [True, False]]

logicExpr2Table = [logicExpr2 x y | x <- [True, False], y <- [True, False]]

-- Exercise 46: Work out the values of these expressions, then check with a
-- list comprehension:
logicExpr3 :: Bool -> Bool -> Bool -> Bool
logicExpr3 a b c = (a /\ b) \/ (a /\ c) ==> a \/ b

logicExpr4 :: Bool -> Bool -> Bool -> Bool
logicExpr4 a b c = (a /\ (b \/c)) \/ (a \/ c) ==> a \/ c

logicExpr3Table =
    [logicExpr3 x y z |
        x <- [True, False],
        y <- [True, False],
        z <- [True, False]]

logicExpr4Table =
    [logicExpr4 x y z |
        x <- [True, False],
        y <- [True, False],
        z <- [True, False]]

-- Exercise 47: Using the Logic data type defined below, define a function
-- distribute that rewrites an expression using the distributive law, and a
-- function deMorgan that does the same for DeMorgan's law.
data Logic = Al | Bl | Cl
           | Andl Logic Logic
           | Orl Logic Logic
           | Notl Logic
           | Implyl Logic Logic
           | Equivl Logic Logic
           deriving (Eq, Show)

distribute :: Logic -> Logic
distribute (Andl a (Orl b c)) = (Orl (Andl a b) (Andl a c))
distribute (Orl a (Andl b c)) = (Andl (Orl a b) (Orl a c))

deMorgan :: Logic -> Logic
deMorgan (Notl (Andl a b)) = (Orl (Notl a) (Notl b))
deMorgan (Notl (Orl a b)) = (Andl (Notl a) (Notl b))
