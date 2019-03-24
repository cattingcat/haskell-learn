module MyMain2 where

import Sub.FixedPointCombinator
import qualified Sub.MyApplicative as MA




interceptSorted :: (Ord a) => [a] -> [a] -> [a]
interceptSorted a b = loop [] a b where 
    loop acc [] _ = acc
    loop acc _ [] = acc
    loop acc a@(ha : ta) b@(hb : tb) 
        | ha == hb = loop (ha : acc) ta tb
        | ha < hb =  loop acc ta b
        | ha > hb =  loop acc a tb

foo :: Int -> Int 
foo i = fix (\rec n -> if n == 0 then 1 else n * rec (n-1)) 5
-- fix :: (a -> a) -> a
-- fix (?? -> Int -> ??) :: Int -> Int
-- fix ( ((Int -> Int) Int) -> Int) :: Int -> Int
-- fix (  (Int -> Int) -> Int -> Int  ) :: Int -> Int
