isFactor :: Integer -> Integer -> Bool
isFactor 0 _ = False
isFactor factor number = rem number factor == 0

factors :: Integer -> [Integer]
factors 0 = []
factors n = [x | x <- [1..n], isFactor x n]

isPerfect :: Integer -> Bool
isPerfect n = sum factors n == n

isAbundant :: Integer -> Bool
isAbundant n = sum . factors n > n

isDeficient :: Integer -> Bool
isDeficient n = sum . factors n < n
