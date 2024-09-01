-- The type a need not be an instance of Eq or Ord. Each node is uniquely
-- identified using the Integer key field.
data SearchTree a = Nub | Cel Integer a (SearchTree a) (SearchTree a)

(==) :: SearchTree a -> SearchTree a -> Bool
Nub == Nub = True
(Cel k d left right) == Nub = False
Nub == (Cel k d left right) = False
(Cel x d left right) == (Cel y d' left' right') = 
    (x == y) && (left == left') && (right == right')
