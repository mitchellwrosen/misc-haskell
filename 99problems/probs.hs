module Foo where

import Data.List

-- 1
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast (x:[_]) = x
myButLast (_:xs) = myButLast xs

-- 3
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = myReverse xs == xs

-- 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a)      = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List [])     = []

compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:ys)
   | x == y    = compress (x:ys)
   | otherwise = x:compress (y:ys)


pack :: Eq a => [a] -> [[a]]
pack (x:xs) = let (y,ys) = span (==x) xs
              in (x:y) : pack ys
pack [] = []

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack

data Count a = Single a | Multiple Int a deriving (Show)
encodeModified :: Eq a => [a] -> [Count a]
encodeModified =
   map (\(x,y) ->
      if x == 1
      then Single y
      else Multiple x y) . encode

decodeModified :: Eq a => [Count a] -> [a]
decodeModified = concatMap countToList where
   countToList (Single a)     = [a]
   countToList (Multiple 0 _) = []
   countToList (Multiple n a) = a:countToList (Multiple (n-1) a)

-- 13
{-encodeDirect :: Eq a => [a] -> [Count a]-}
{-encodeDirect x = let (ys,zs) = span (==x) xs-}
                 {-in c : encodeDirect zs where-}
                  {-c = case length ys of-}
                     {-1 -> Single-}
                     {-otherwise -> Multiple (length ys) x-}

-- 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = dropEvery' xs n n

dropEvery' :: [a] -> Int -> Int -> [a]
dropEvery' [] _ _ = []
dropEvery' (_:xs) n 1 = dropEvery' xs n n
dropEvery' (x:xs) n m = x:dropEvery' xs n (m-1)

split :: [a] -> Int -> ([a], [a])
split xs n = (take' n xs, drop' n xs) where
   take' 0 _ = []
   take' m (y:ys) = y:take' (m-1) ys

   drop' 0 ys = ys
   drop' m (_:ys) = drop (m-1) ys

slice :: [a] -> Int -> Int -> [a]
slice xs from to
   | from > 0 = take (to - from + 1) $ drop (from-1) xs

rotate :: [a] -> Int -> [a]
rotate xs n
   | n >= 0 = drop n xs ++ take n xs
   | n < 0  = drop (length xs - (-n)) xs ++ take (length xs - (-n)) xs

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs!!(n-1), take (n-1) xs ++ drop n xs)

-- 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = let (ys, zs) = split xs (n-1) in ys ++ x:zs

-- 22
range :: Int -> Int -> [Int]
range n m
   | n == m = [n]
   | n < m  = n : range (n+1) m

-- 23

-- 24

-- 25

-- 26 -- TODO: fix
combinations :: Eq a => Int -> [a] -> [[a]]
combinations 0 _      = [[]]
combinations _ []     = [[]]
combinations n (x:xs) =
   let
      left       = map (x:) (combinations (n-1) xs)
      right      = combinations n xs
      notNull    = filter (not . null)
   in
      notNull left ++ notNull right

-- 31
isPrime :: Int -> Bool
isPrime n = n == last (primeSieve n)

primeSieve :: Int -> [Int]
primeSieve n = primeSieve' (floor . sqrt . fromIntegral $ n)  [1..n]

-- removes multiples from 2 up to n
primeSieve' :: Int -> [Int] -> [Int]
primeSieve' n xs
   | n == 1 = [1]
   | n == 2 = removeMultiplesOf 2 xs
   | n > 2  = removeMultiplesOf n (primeSieve' (n-1) xs)

removeMultiplesOf :: Int -> [Int] -> [Int]
removeMultiplesOf 1 _  = []
removeMultiplesOf _ [] = []
removeMultiplesOf n xs = filter (not . isMultipleOf n) xs

isMultipleOf :: Int -> Int -> Bool
isMultipleOf n k
   | n == k    = False
   | otherwise = k `mod` n == 0

-- 32
myGCD :: Int -> Int -> Int
myGCD x y
   | y == 0    = x
   | otherwise = myGCD y (x `mod` y)

-- 33
coprime :: Int -> Int -> Bool
coprime x y = myGCD x y == 1

-- 34
totient :: Int -> Int
totient x = length $ filter (coprime x) [1..(x-1)]

-- 35
primeFactors :: Int -> [Int]
primeFactors n = [x | x <- factors n, x `elem` primeSieve n]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

-- 46
table :: (Bool -> Bool -> Bool) -> IO ()
table f = do
      putStrLn $ getRow True True
      putStrLn $ getRow True False
      putStrLn $ getRow False True
      putStrLn $ getRow False False
      where getRow a b = show a ++ " " ++ show b ++ " " ++ show (f a b)

-- 49
gray :: Int -> [String]
gray 1 = ["0","1"]
gray n = map ('0':) (gray (n-1)) ++ map ('1':) (reverse $ gray (n-1))

--50
data HuffTree a = EmptyH
                | BranchH { symbol :: [a]
                         , freq    :: Int
                         , leftH   :: HuffTree a
                         , rightH  :: HuffTree a
                         }
              deriving (Show, Eq)

instance Eq a => Ord (HuffTree a) where
   x `compare` y
      | freq x < freq y = LT
      | freq x > freq y = GT
      | otherwise       = EQ

-- Gets a HuffTree given a list of (sym,freq) tuples
huffTree :: Eq a => [(a,Int)] -> HuffTree a
huffTree = huffTree' . sort . map huffLeaf where
   huffTree' []     = EmptyH
   huffTree' [x]    = x
   huffTree' (x:y:ys) = (huffTree' . sort) (combine x y : ys) where
      combine l r = BranchH (symbol l ++ symbol r) (freq l + freq r) l r

   huffLeaf (s,n) = BranchH [s] n EmptyH EmptyH

huffman :: Eq a => [(a,Int)] -> [(a,String)]
huffman = huffman' "" . huffTree where
   huffman' _ EmptyH = []
   huffman' s (BranchH [x] _ _ _) = [(x,s)] -- Leaf
   huffman' s (BranchH _   _ l r) =
      huffman' ('0':s) l ++ huffman' ('1':s) r

-- 55
data Tree a = Empty | Branch { val   :: a
                             , left  :: Tree a
                             , right :: Tree a
                             }
              deriving Eq

instance Show a => Show (Tree a) where
   show Empty = "Empty"
   show (Branch a l r) =
      "Branch " ++ show a ++ " (" ++ show l ++ ") (" ++ show r ++ ")"

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = nub $ concatMap addNode (cbalTree (n-1))
 where
   addNode Empty = [Branch 'x' Empty Empty]
   addNode (Branch v l r)
      | treeLen l > treeLen r = addRight
      | treeLen l < treeLen r = addLeft
      | otherwise             = addLeft ++ addRight
      where addLeft  = [Branch v l' r  | l' <- addNode l]
            addRight = [Branch v l  r' | r' <- addNode r]

treeLen :: Tree a -> Int
treeLen Empty = 0
treeLen (Branch _ l r) = 1 + treeLen l + treeLen r

-- 56
symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r
  where
   mirror Empty Empty = True
   mirror Empty _     = False
   mirror _     Empty = False
   mirror (Branch _ l1 r1) (Branch _ l2 r2) = mirror l1 r2 && mirror l2 r1

-- 57
construct :: Ord a => [a] -> Tree a
construct = foldl insert' Empty
  where
   insert' Empty x = Branch x Empty Empty
   insert' t@(Branch x l r) y =
      case compare x y of
         LT -> Branch x l (insert' r y)
         GT -> Branch x (insert' l y) r
         EQ -> t

-- 58
symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree

-- 59
-- in other file
