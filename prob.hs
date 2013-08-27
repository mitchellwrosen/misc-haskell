module Foo where

import Data.Ratio
import Control.Arrow

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

instance Functor Prob where
   fmap f (Prob xs) = Prob $ map (first f) xs


