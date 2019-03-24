module Sub.Trees.RandomTree where

import Sub.Rnd.MyRandom
import Sub.Trees.Tree
import Sub.MyState

randomTree :: Int -> Int -> Random (Tree Int)
randomTree d c = State $ \s -> let
    (depth:child:value:[], s2) = runState (rndLimits [d, c, 100]) s
    (children, s3) = if d == 1 then ([], s2)
        else runState (times child (randomTree (d - 1) c)) s2
    in (Node value children, s3)


randomT :: Int -> Int -> Random (Tree Int)
randomT d c = do
    tmp <- rndLimits [d, c, 100]
    let (depth:child:value:[]) = tmp
    children <- if d == 1 then pure ([]) else times child (randomT (d - 1) c)
    return $ Node value children


times :: Monad m => Int -> m a -> m [a]
times 0 s = pure []
times l s = s >>= (\a -> (times (l - 1) s) >>= (\c -> pure (a:c)))

rndLimits :: [Int] -> Random [Int]
rndLimits [] = pure []
rndLimits (lim:rest) = limIntRnd lim >>= \r -> (rndLimits rest) >>= \l -> pure (r:l)

runRndTree = runState (randomT 5 5) 0.534

