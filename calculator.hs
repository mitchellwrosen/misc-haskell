module Main where

import Text.Parsec
import Control.Monad.Identity

parseNumber :: ParsecT String u Identity Int
parseNumber = do
   neg <- (char '-' >> return "-") <|> return ""
   n' <- many1 $ oneOf "0123456789"
   return (read (neg ++ n') :: Int)

calculation :: ParsecT String u Identity Int
calculation = parseNumber

calculate :: String -> String
calculate s =
   case ret of
      Left e  -> "error: " ++ show e
      Right n -> "answer: " ++ show n
   where
      ret = parse calculation "" s


main :: IO ()
main = interact $ unlines . map calculate . lines
