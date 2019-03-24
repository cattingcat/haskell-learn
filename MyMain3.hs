module MyMain3 where

import Sub.MyNat

(+-+) :: Int -> Int -> Int
(+-+) a b = (+) a b

(+*+) :: Int -> Int -> Int
(+*+) a b = (*) a b

zoo :: Int -> Int -> Int -> Int
zoo a b c = a +-+ b +*+ c 


infixl 6 +-+
infixl 7 +*+

{-
    infixl:
    (((a op b) op c) op d)

    infixr:
    (a op (b op (c op d)))

    ((1 - 2) - 3) = -1 - 3 = -4
    (1 - (2 - 3)) = 1 - (-1) = 2
-}

