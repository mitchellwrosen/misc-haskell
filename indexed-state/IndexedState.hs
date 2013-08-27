module IndexedState where

import Prelude hiding (fmap, (>>=), (>>), return)

newtype IState i o a = IState { runIState :: i -> (a, o) }

evalIState :: IState i o a -> i -> a
evalIState state input = fst $ runIState state input

execIState :: IState i o a -> i -> o
execIState state input = snd $ runIState state input

return :: a -> IState s s a
return a = IState $ \s -> (a, s)

fmap :: (a -> b) -> IState i o a -> IState i o b
fmap f state = IState $ \input -> let (a, output) = runIState state input
                                 in (f a, output)

join :: IState i m (IState m o a) -> IState i o a
join state = IState $ \input -> let (a, output) = runIState state input
                               in runIState a output

(>>=) :: IState i m a -> (a -> IState m o b) -> IState i o b
state >>= f = IState $ \input -> let (a, output) = runIState state input
                               in runIState (f a) output

(>>) :: IState i m a -> IState m o b -> IState i o b
v >> w = v >>= \_ -> w

fail :: String -> IState i o a
fail str = error str

get :: IState s s s
get = IState $ \s -> (s, s)

put :: o -> IState i o ()
put o = IState $ \_ -> ((), o)

modify :: (i -> o) -> IState i o ()
modify f = IState $ \i -> ((), f i)
