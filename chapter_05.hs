data BinTree a =
    BinLeaf |
    BinNode a (BinTree a) (BinTree a)
    deriving Show

inorder :: BinTree a -> [a]
inorder BinLeaf = []
inorder (BinNode x t1 t2) = inorder t1 ++ [x] ++ inorder t2

preorder :: BinTree a -> [a]
preorder BinLeaf = []
preorder (BinNode x t1 t2) = [x] ++ preorder t1  ++ preorder t2

postorder :: BinTree a -> [a]
postorder BinLeaf = []
postorder (BinNode x t1 t2) = postorder t1  ++ postorder t2 ++ [x]

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

