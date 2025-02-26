data BinTree a =
    BinLeaf |
    BinNode a (BinTree a) (BinTree a)
    deriving Show

-- The tree flattening functions below have a time complexity of n² - if the
-- assumption is made that the time complexity of ++ is n, as determined by
-- the size of its left operand.
inorder :: BinTree a -> [a]
inorder BinLeaf = []
inorder (BinNode x t1 t2) = inorder t1 ++ [x] ++ inorder t2

preorder :: BinTree a -> [a]
preorder BinLeaf = []
preorder (BinNode x t1 t2) = [x] ++ preorder t1  ++ preorder t2

postorder :: BinTree a -> [a]
postorder BinLeaf = []
postorder (BinNode x t1 t2) = postorder t1  ++ postorder t2 ++ [x]

-- This implementation of inorder flattening should be more efficient - with
-- time complexity n due to its usage of : as opposed to ++.
inorderEfficient :: BinTree a -> [a]
inorderEfficient tree = g tree []
    where
        g BinLeaf continuation = continuation
        g (BinNode x l r) continuation = g l (x : g r continuation)

-- Exercise 1: Define a Haskell datatype Tree1 for a tree that contains a
-- character and an integer in each node, along with exactly three subtrees.

data Tree1 =
    Tree1Leaf |
    Tree1Node Char Int Tree1 Tree1 Tree1
    deriving Show

-- Exercise 2: Define a Haskell datatype Tree2 for a tree that contains an
-- integer in each node, and that allows each node to have any number of
-- subtrees.

data Tree2 =
    Tree2Leaf |
    Tree2Node Int [Tree2]

-- Exercise 3: Suppose that a tree has type BinTree a and we have a function
-- f :: a -> b. Write a new traversal function
-- inorderf :: (a -> b) -> BinTree a -> [b] that traverses the tree using
-- inorder, but it applies f to the data value in each node before placing the
-- result in the list.
inorderf :: (a -> b) -> BinTree a -> [b]
inorderf f tree = map f (inorder tree)

reflect :: BinTree a -> BinTree a
reflect BinLeaf = BinLeaf
reflect (BinNode x l r) = BinNode x (reflect r) (reflect l)

-- Distance between a root and its deepest leaf.
height :: BinTree a -> Integer
height BinLeaf = 0
height (BinNode _ l r) = 1 + max (height l) (height r)

-- Number of nodes in a BinTree.
size :: BinTree a -> Integer
size BinLeaf = 0
size (BinNode _ l r) = 1 + size l + size r

-- Is a BinTree balanced?
balanced :: BinTree a -> Bool
balanced BinLeaf = True
balanced (BinNode _ l r) = balanced l && balanced r && (height l == height r)

-- Exercise 5: Define two trees of size seven, one with the largest possible
-- height and the other with the smallest possible height.

treeUnbalanced :: BinTree Int
treeUnbalanced = BinNode 1
    BinLeaf
    (BinNode 2
        BinLeaf
        (BinNode 3
            BinLeaf
            (BinNode 4
                BinLeaf
                (BinNode 5
                    BinLeaf
                    (BinNode 6
                        BinLeaf
                        (BinNode 7
                            BinLeaf
                            BinLeaf))))))

treeBalanced :: BinTree Int
treeBalanced  = BinNode 1
    (BinNode 2
        (BinNode 3 BinLeaf BinLeaf)
        (BinNode 4 BinLeaf BinLeaf))
    (BinNode 5
        (BinNode 6 BinLeaf BinLeaf)
        (BinNode 7 BinLeaf BinLeaf))

-- Datatype describing a simple mathematical expression tree.
data Exp =
    Const Integer |
    Add Exp Exp |
    Mult Exp Exp

eval :: Exp -> Integer
eval (Const n) = n
eval (Add e1 e2) = eval e1 + eval e1
eval (Mult e1 e2) = eval e1 * eval e1

-- Search inside a binary search tree.
bstSearch :: Ord a => a -> BinTree (a,b) -> Maybe b
bstSearch key BinLeaf = Nothing
bstSearch key (BinNode (a,b) l r) =
    if key == a
        then Just b
        else if key < a
            then bstSearch key l
            else bstSearch key r

-- Insert a node into a binary search tree.
bstInsert :: Ord a => (a,b) -> BinTree (a,b) -> BinTree (a,b)
bstInsert (k,v) BinLeaf = BinNode (k,v) BinLeaf BinLeaf
bstInsert (k,v) (BinNode (x,y) l r) =
    if k == x
        then (BinNode (k,v) l r)
        else if k < x
            then BinNode (x,y) (bstInsert (k,v) l) r
            else BinNode (x,y) l (bstInsert (k,v) r)

-- Exercise 8: Define a function mapTree that takes a function and applies it
-- to every node in the tree, returning a new tree of results.

mapTree :: (a->b) -> BinTree a -> BinTree b
mapTree _ BinLeaf = BinLeaf
mapTree f (BinNode x l r) = BinNode (f x) (mapTree f l) (mapTree f r)

-- Exercise 9: Write concatTree, a function that takes a tree of lists and
-- concatenates the lists in order from left to right.

concatTree :: BinTree [a] -> [a]
concatTree tree = concat (inorderEfficient tree)

-- Exercise 10: Write zipTree, a function that takes two trees and pairs each
-- of the corresponding elements in a list. Your function should return
-- Nothing if the two trees do not have the same shape. For example,
--  zipTree (Node 2 (Node 1 Tip Tip) (Node 3 Tip Tip))
--          (Node 5 (Node 4 Tip Tip) (Node 6 TIp Tip))
--  ==> Just [(1,4)(2,5)(3,6)]

sameShape :: BinTree a -> BinTree b -> Bool
sameShape BinLeaf BinLeaf = True
sameShape BinLeaf (BinNode _ _ _) = False
sameShape (BinNode _ _ _) BinLeaf = False
sameShape (BinNode _ l1 r1) (BinNode _ l2 r2) =
    (sameShape l1 l2) && (sameShape r1 r2)

zipTree :: BinTree a -> BinTree b -> Maybe [(a,b)]
zipTree t1 t2 =
    if sameShape t1 t2
        then Just (zip (inorderEfficient t1) (inorderEfficient t2))
        else Nothing

-- Exercise 11: Write zipWithTree, a function that is like zipWith except that
-- it takes trees instead of lists. The first argument is a function of type
-- a -> b -> c, the second argument is a tree with elements of type a, and the
-- third argument is a tree with elements of type b. The function returns a
-- list with type [c].

zipWithTree :: (a -> b -> c) -> BinTree a -> BinTree b -> Maybe [c]
zipWithTree f t1 t2 =
    if sameShape t1 t2
        then Just (zipWith f (inorderEfficient t1) (inorderEfficient t2))
        else Nothing

-- Exercise 12: Write appendTree, a function that takes a binary tree and a
-- list, and appends the contents of the tree (traversed from left to right)
-- to the front of the list. For example,
--  appendTree (BinNode 2 (BinNode 1 BinLeaf BinLeaf)
--                        (BinNode 3 BinLeaf BinLeaf)
--             [4,5]
-- evaluates to [1,2,3,4,5]. Try to find an efficient solution that minimises
-- recopying.

appendTree :: BinTree a -> [a] -> [a]
appendTree t xs = (inorderEfficient t) ++ xs

