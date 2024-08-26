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

