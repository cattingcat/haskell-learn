module Sub.Stream where
import Prelude(String(..), Show(..), Int(..), Bool(..), (+), (-), (++))

data Stream a = a :& Stream a

id :: a -> a
id a = a

(.) :: (b -> c) -> (a -> b) -> (a -> c)
g . f = \x -> g(f(x))

constStream :: a -> Stream a
constStream a = next id a

ints :: Stream Int
ints = next (+1) 0

next :: (a -> a) -> a -> Stream a
next f z = z :& next f (f z)

take :: Stream a -> Int -> [a]
take s 0        = []
take (a :& t) n = a : take t (n-1)

head :: Stream a -> a
head (a :& _) = a

tail :: Stream a -> Stream a
tail (_ :& t) = t

(!!) :: Stream a -> Int -> a
s !! 0 = head s
s !! n = (tail s) !! (n - 1)

map :: (a -> b) -> Stream a -> Stream b
map f (h :& t) = (f h) :& (map f t)

filter :: (a -> Bool) -> Stream a -> Stream a
filter f (h :& t) = if (f h) then h :& ft else ft
    where ft = filter f t

instance Show a => Show (Stream a) where 
    show s = "(" ++ (subShow 5 "" s) ++ " ...)" where 
        subShow 1 l (h' :& _) = (l ++ show h')
        subShow n l (h' :& t') = subShow (n - 1) (l ++ show h' ++ " ") t'

instance Show (a -> b) where 
    show f = "qweqwe"