quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
   let left = quicksort (filter (<=x) xs)
       right = quicksort (filter (>x) xs)
   in left ++ [x] ++ right

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
   where p x = x `mod` 3829 == 0

takeWhile' p [] = []
takeWhile' p (x:xs)
   | p x       = x : takeWhile' p xs
   | otherwise = []

filter' p [] = []
filter' p (x:xs)
   | p x       = x : filter' p xs
   | otherwise = filter' p xs

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz a
   | even a = a : collatz (a `div` 2)
   | odd a = a : collatz (3 * a + 1)

sum' = foldl1 (+)

elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

reverse' = foldl (flip (:)) []

intersperse' _ [] = []
intersperse' e [x] = [x]
intersperse' e (x:xs) = x : e : intersperse' e xs

intercalate' l ll
