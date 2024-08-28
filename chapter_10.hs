import Stdm

data Colour = Red | Blue | Green | Orange | Yellow | Violet
    deriving (Eq, Show)

colourComplement :: Digraph Colour
colourComplement =
    ([Red, Blue, Green, Orange, Yellow, Violet],
     [
        (Red, Green),
        (Green, Red),
        (Blue, Orange),
        (Orange, Blue),
        (Yellow, Violet),
        (Violet, Yellow)
     ])

-- Exercise 1: Work out the values of the following expressions, and then
-- check your answer by evaluating the expressions with the computer.
e1d = domain [(1,100),(2,200),(3,300)]
e1c = codomain [(1,100),(2,200),(3,300)]
e1cp = crossproduct [1,2,3] [4]

-- Exercise 10: Determine by hand whether the following relations are
-- transitive, and then check your conclusion using the computer:
e10a = isTransitive ([1,2],[(1,2),(2,1),(2,2)])
e10b = isTransitive ([1,2,3],[(1,2)])

-- Exercise 13: First work out by hand the ordered pairs in the following
-- relational compositions, and then check your results using the computer:
e13a = relationalComposition [(1,2),(2,3)] [(3,4)]
e13b = relationalComposition [(1,2)] [(1,3)]

-- Exercise 15: Work out the values of these expressions, and then evaluate
-- them using a computer:
e15a = equalityRelation [1,2,3]
e15b = equalityRelation ([]::[Int])

-- Exercise 16: Calculate the following relational powers by hand, and then
-- evaluate them using the computer.
e16a = relationalPower ([1,2,3,4], [(1,2),(2,3),(3,4)]) 1
e16b = relationalPower ([1,2,3,4], [(1,2),(2,3),(3,4)]) 2
e16c = relationalPower ([1,2,3,4], [(1,2),(2,3),(3,4)]) 3
e16d = relationalPower ([1,2,3,4], [(1,2),(2,3),(3,4)]) 4

-- Exercise 24: Work out the following reflexive closures by hand, and then
-- check your results using the computer:
e24a = reflexiveClosure ([1,2,3],[(1,2),(2,3)])
e24b = reflexiveClosure ([1,2],[(1,2),(2,1)])

-- Exercise 26: Work out the following symmetric closures by hand, and then
-- calculate them using the computer:
e26a = symmetricClosure ([1,2],[(1,1),(1,2)])
e26b = symmetricClosure ([1,2,3],[(1,2),(2,3)])

-- Exercise 29: Work out the following symmetric closures by hand, and then
-- calculate them using the computer:
e29a = transitiveClosure ([1,2,3],[(1,2),(2,3)])
e29b = transitiveClosure ([1,2,3],[(1,2),(2,1)])

-- Exercise 32: Work out by hand whether the following digraphs are partial
-- orders, and then check your results using the computer:
e32a = isPartialOrder ([1,2,3],[(1,2),(2,3)])
e32b = isPartialOrder ([1,2,3],[(1,2),(2,3),(1,3),(1,1),(2,2),(3,3)])

-- Exercise 33: Calculate the following by hand, and then evaluate using the
-- computer:
e33a = isWeakest [(1,2),(2,3),(1,3),(1,1),(2,2),(3,3)] 2
e33b = isWeakest [(1,2),(1,3),(1,1),(2,2),(3,3)] 3

e33c = isGreatest [(1,2),(2,3),(1,3),(1,1),(2,2),(3,3)] 3
e33d = isGreatest [(1,2),(1,3),(1,1),(2,2),(3,3)] 1

-- Exercise 38: Evaluate the following expressions by hand and using the
-- computer:
e38a = isLinearOrder ([1,2,3],[(1,2),(2,3),(1,3),(1,1),(2,2),(3,3)])
e38b = isLinearOrder ([1,2,3],[(1,2),(1,3),(1,1),(2,2),(3,3)])

-- Exercise 41: Check to see that the following partial orders are not, in
-- fact, topological orders. Use the computer to generate a total order, using
-- a topological sort.
e41a = topsort (
    [1,2,3,4],
    [(1,2),(1,3),(2,3),(1,4),(2,4),(1,1),(2,2),(3,3),(4,4)])

e41b = topsort ( [1,2,3],[(1,2),(1,3),(1,4),(1,1),(2,2),(3,3)])

-- Exercise 42: Evaluate the following expressions using the computer:
e42a = equivalenceRelation ([1,2],[(1,1),(2,2),(1,2),(2,1)])
e42b = equivalenceRelation ([1,2,3],[(1,1),(2,2)])

e42c = isEquivalenceRelation ([1,2],[(1,1),(2,2),(1,2),(2,1)])
e42d = isEquivalenceRelation ([1],[])

-- Exercise 43: Does the topological sort require that the graph's relation is
-- a partial order?

-- Yes - the topological sort can be defined as the construction of a total
-- order from a partial order.

-- Exercise 44: Can the graph given to a topological sort have cycles?

-- No. The graph must be a partial order, which has no cycles by definition.

-- Exercise 48: Write a function that takes a relation and returns True if
-- that relation has a power that is the given relation.

powerCheck :: (Eq a, Show a) => Digraph a -> Bool
powerCheck d = comparePower d 2
    where
        comparePower (set, relation) pow
            | length set > (pow + 1) = False
            | otherwise =
                if (relationalPower (set, relation) pow) == relation
                    then True
                    else comparePower (set, relation) (pow + 1)

