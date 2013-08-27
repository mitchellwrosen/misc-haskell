module Foo where

import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Char (isAlpha, isNumber, isPunctuation)

liftM :: Monad m => (a -> r) -> m a -> m r
liftM f = (=<<) (return . f)

getValidPassword :: MaybeT IO String
getValidPassword = do
   s <- lift getLine
   guard (isValid s)
   return s

askPassword :: MaybeT IO ()
askPassword = do
   lift $ putStrLn "Insert password:"
   password <- getValidPassword
   lift $ putStrLn password

isValid :: String -> Bool
isValid s = length s >= 8 &&
            any isAlpha s &&
            any isNumber s &&
            any isPunctuation s
