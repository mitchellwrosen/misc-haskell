rpn :: String -> Float
rpn = head . foldl f [] . words
   where f (x:y:ys) "*" = (x * y):ys
         f (x:y:ys) "+" = (x + y):ys
         f (x:y:ys) "-" = (y - x):ys
         f (x:y:ys) "/" = (y / x):ys
         f (x:y:ys) "^" = (y ** x):ys
         f (x:xs) "ln" = log x:xs
         f xs "sum" = [sum xs]
         f xs numberString = read numberString:xs
