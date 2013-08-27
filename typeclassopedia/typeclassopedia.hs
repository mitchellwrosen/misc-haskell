{-# LANGUAGE DatatypeContexts #-}

module Typeclassopedia where

import Control.Applicative

{-instance Functor ((->) r) where-}
   {-fmap g f = g . f-}

data Either_ a b = Left_ a
                 | Right_ b

instance Functor (Either_ e) where
   fmap _ (Left_ e)  = Left_ e
   fmap g (Right_ a) = Right_ $ g a

{-instance Functor ((,) e) where-}
   {-fmap g (e, a) = (e, g a)-}

data Pair a = Pair a a

instance Functor Pair where
   fmap g (Pair a b) = Pair (g a) (g b)

data ITree a = Leaf (Int -> a)
             | Node [ITree a]

instance Functor ITree where
   fmap g (Leaf h)  = Leaf (g . h)
   fmap g (Node xs) = Node $ fmap (fmap g) xs

data Maybe_ a = Nothing_
              | Just_ a

instance Functor Maybe_ where
   fmap _ Nothing_ = Nothing_
   fmap g (Just_ x) = Just_ (g x)

instance Applicative Maybe_ where
   pure = Just_
   Nothing_ <*> _ = Nothing_
   Just_ f <*> x = fmap f x

{-instance Monad [] where-}
   {-return x = [x]-}
   {-xs >>= f = concat $ fmap f xs-}

{-instance Monad ((->) r) where-}
   {-return = const-}
   {-f >>= g = \r -> g (f r) r-}

data Free_ f a = Var_ a
               | Node_ (f (Free_ f a))

{-instance Functor f => Functor (Free_ f) where-}
   {-fmap g (Var_ a) = Var_ (g a)-}
   {-fmap g (Node_ f) = Node_ (fmap g f)-}

join' :: Monad m => m (m a) -> m a
join' = (>>= id)

fmap' :: Monad m => (a -> b) -> m a -> m b
fmap' g m = m >>= return . g

