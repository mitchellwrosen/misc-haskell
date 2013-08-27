data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

makeTree :: a -> Tree a
makeTree x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = makeTree x
treeInsert x (Node a leftTree rightTree)
   | x == a = Node x leftTree rightTree
   | x < a = Node a (treeInsert x leftTree) rightTree
   | x > a = Node a leftTree (treeInsert x rightTree)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a leftTree rightTree)
   | x == a = True
   | x < a = treeElem x leftTree
   | x > a = treeElem x rightTree
