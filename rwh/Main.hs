module Main where

import Json

main :: IO ()
main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])
