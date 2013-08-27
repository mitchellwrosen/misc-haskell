toUpper :: Char -> Char
toUpper

main = do
   putStrLn "first name?"
   f <- getLine
   putStrLn "last name?"
   l <- getLine
   let bigf = map toUpper f
       bigl = map toUpper l
   putStrLn $ "hey " ++ bigf ++ " " ++ bigl
