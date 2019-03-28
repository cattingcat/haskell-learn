module Sub.Trees.Tree where

data Tree a = Node a [Tree a]

instance Show a => Show (Tree a) where
    show t@(Node r l) = show' "" t where 
        show' offset (Node r l) = offset ++ (show r) ++ showChildren where
            stOff = offset ++ "  "
            showChildren = foldr (\i a -> a ++ "\n" ++ (show' stOff i)) "" l

instance Functor Tree where 
    fmap f (Node t l) = Node (f t) (fmap (\x -> fmap f x) l)

bind' :: Tree a -> (a -> Tree b) -> Tree b
bind' (Node t l) f = let (Node r nl) = (f t) 
    in Node r (nl ++ (fmap (\x -> bind' x f) l))

instance Applicative Tree where 
    pure a = Node a []
    (<*>) sf sa = bind' sf (\f -> fmap f sa)

instance Monad Tree where 
    (>>=) = bind'


foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (Node a children) = f a ((foldTree f) <$> children)

strTree x = Node (show x ++"h") [Node (show x ++ "l") [], Node (show x ++ "r") []]

funcTree :: Show x => Tree (x -> String)
funcTree = Node (\x -> (show x) ++ " root") [Node (\x -> (show x) ++ " left") [], Node (\x -> (show x) ++ " right") []]


testTree = Node 1 [Node 2 [], Node 3 []]
r1 = bind' testTree strTree
r2 = funcTree <*> testTree