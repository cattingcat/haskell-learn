module Sub.Fsm.Speaker where

import Sub.MyState

type Fsm s = State s s


fsm :: (ev -> s -> s) -> (ev -> Fsm s)
fsm trans = \ev -> State (\s -> (s, trans ev s))

data Level = Level Int deriving (Show)
data SpeakerState = On | Off deriving (Show)

type Speaker = (SpeakerState, Level)

data UserAction = Button | Louder | Quiter deriving (Show)

level :: Int -> Level
level a 
    | a > 10    = Level 10
    | a < 0     = Level 0
    | otherwise = Level a

quiter :: Level -> Level
quiter (Level a) = Level $ max 0 (a - 1)

louder :: Level -> Level
louder (Level a) = Level $ min 10 (a + 1)

speaker :: UserAction -> Fsm Speaker
speaker = fsm $ trans where
    trans :: UserAction -> Speaker -> Speaker
    trans Button (On, a) = (Off, a)
    trans Button (Off, a) = (On, a)
    trans Louder (a, l) = (a, louder l)
    trans Quiter (a, l) = (a, quiter l)

mapM' :: Monad m => [a] -> (a -> m b) -> m [b]
mapM' [] _ = pure []
mapM' (h:t) f = (f h) >>= (\hr -> (mapM' t f) >>= (\tr -> pure (hr:tr)))

res = mapM' [Button, Louder, Quiter, Button, Louder] speaker

r = runState res (On, Level 5)