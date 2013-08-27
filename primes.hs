primeNum :: Integer -> Integer
primeNum = last . primesTo

primesTo :: Integer -> [Integer]
primesTo n = 2 : sieve [3,5..n] where
   sieve (x:xs) | x*x > n   = x : xs
                | otherwise = x : sieve (xs `minus` [x*x, x*x + 2*x..])

minus :: (Ord a) => [a] -> [a] -> [a]
minus xs [] = xs
minus [] xs = []
minus (x:xs) (y:ys) =
   case x `compare` y of
      LT -> x : minus xs (y:ys)
      EQ -> minus xs ys
      GT -> minus (x:xs) ys
