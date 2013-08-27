module BST where

data RedBlackTree a = Empty
                    | Leaf
                    | Node { val         :: a
                           , color       :: Color
                           , left        :: RedBlackTree a
                           , right       :: RedBlackTree a
                           , parent      :: RedBlackTree a
                           }
                    deriving Eq

data Color = Red | Black deriving (Eq, Show)

instance Show a => Show (RedBlackTree a) where
   show Empty = "Empty"
   show Leaf  = "Leaf"
   show (Node c v l r _ ) =
      "(" ++ show c ++ " " ++ show v ++ " " ++ show l ++ " " ++ show r ++ ")"

grandparent :: RedBlackTree a -> RedBlackTree a
grandparent (Node _ _ _ _ p) = parent p
grandparent _ = Empty

uncle :: Eq a => RedBlackTree a -> RedBlackTree a
uncle (Node _ _ _ _ p)
   | g == Empty    = Empty
   | left g == p   = right g
   | otherwise     = left g
   where g = parent p

insert :: Ord a => RedBlackTree a -> a -> RedBlackTree a
insert Empty v = Node v Red Leaf Leaf Empty
insert t@(Node v c l r p) v2 =
   case compare v v2 of
      LT -> Node v c l (insert r v2) p
      GT -> Node v c (insert l v2) r p
      EQ -> t
