module Sub.Trees.RandomTree where

import Sub.Rnd.MyRandom
import Sub.Trees.Tree
import Sub.MyState

randomTree :: Int -> Int -> Random (Tree Int)
randomTree d c = State $ \s -> let
    (depth, s0) = runState (limIntRnd d) s
    (child, s1) = runState (limIntRnd c) s0
    (value, s2) = runState (limIntRnd 100) s1
    (children, s3) = if d == 1 then ([], s2)
        else runState (times child (randomTree (d - 1) c)) s2
    in (Node value children, s3)

times :: Monad m => Int -> m a -> m [a]
times 0 s = pure []
times l s = s >>= (\a -> (times (l - 1) s) >>= (\c -> pure (a:c)))

runRndTree = runState (randomTree 5 5) 0.534

