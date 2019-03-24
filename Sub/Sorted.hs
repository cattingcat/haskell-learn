module Sub.Sorted(Sorted, sorted, s2l) where

import Data.List

newtype Sorted a = Sorted [a] deriving(Show)

sorted :: Ord a => [a] -> Sorted a
sorted l = Sorted $ sort l

s2l :: Sorted a -> [a]
s2l (Sorted l) = l