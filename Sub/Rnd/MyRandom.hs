module Sub.Rnd.MyRandom where

import Sub.MyState

type Random a = State Double a

next :: Random Double
next = State $ \s -> (s, nextRandom s)

nextRandom :: Double -> Double
nextRandom = snd . properFraction . (105.947 * )

addRandom :: Double -> Random Double
addRandom a = fmap (+a) next

limIntRnd :: Int -> Random Int
limIntRnd lim = fmap (round . (*(fromIntegral lim))) next


data Coin = Heads | Tails deriving (Show)

dropCoin :: Random Coin
dropCoin = fmap toCoin next where
    toCoin a 
        | a > 0.5 = Heads
        | a <= 0.5 = Tails

runRnd = runState (addRandom 5) 0.5

runCoin = runState dropCoin 0.9