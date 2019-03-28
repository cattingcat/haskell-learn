module Sub.Lazy.Laziness where


iterate' :: (a -> a) -> a -> [a]
iterate' f z = let next = f z in next : iterate' f next


test = take 5 (iterate' (+1) 5)


data Px a = a :+: Px a deriving (Show, Eq)

infixr :+:

eval :: Num a => Int -> Px a -> a -> a
eval 0 _ _ = 0
eval n (p :+: pa) v = p + v * (eval (n - 1) pa v)

p0 :: Num a => Px a
p0 = 0 :+: p0

ps :: Num a => [a] -> Px a
ps [] = p0
ps (h:t) = h :+: ps t

(.*) :: Num a => a -> Px a -> Px a
(.*) a (b :+: pb) = (a * b) :+: (a .* pb)

instance Num a => Num (Px a) where 
    (+) (a :+: pa) (b :+: pb) = (a + b) :+: (pa + pb)
    (-) (a :+: pa) (b :+: pb) = (a - b) :+: (pa - pb)
    (*) f@(a :+: pa) g@(b :+: pb) = (a * b) :+: (a .* pb + pa * g) 
    abs = undefined
    signum = undefined
    fromInteger i = (fromInteger i) :+: p0

{-
    Q = F / G
    F = Q*G
    f + xF' = (q + xQ')(g + xG')
    f + xF' = q g + q xG' + g xQ' + xG' * xQ'
    f + xF' = q g + q xG' + xQ' (g  + xG')
    f + xF' = q g + q xG' + xQ' G
    f + xF' = q g + x (q G' + Q' G)
    f = q * g 
    F' = q G' + Q' G
    q = f/g
    Q' = (F' - q G') / G
-}
instance (Eq a, Fractional a) => Fractional (Px a) where
    (/) (f :+: fs) gg@(g :+: gs) =  q :+: ((fs - (q .* gs)) / gg) where q = f / g
    fromRational r = (fromRational r) :+: p0


diff :: Num a => Px a -> Px a
diff (a :+: px) = diff' 1 px where 
    diff' p (a :+: px) = (a * p) :+: diff' (p + 1) px

int :: Fractional a => Px a -> Px a
int (pa) = 0 :+: (int' 1 pa) where
    int' n (a :+: pa) = (a / n) :+: (int' (n+1) pa)

test2 :: Int 
test2 = eval 10 (1 :+: 2 :+: p0) 5



{-
    lazySafeHead :: [a] -> Maybe a
    lazySafeHead ~(x:xs) = Just x      -- ~ neans  that this pattern is always true (but if it isnt - you will receive runtime error)
    lazySafeHead [] = Nothing          -- never called
-}