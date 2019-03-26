import Control.Applicative


f :: Char -> Char -> Char -> String
f a b c = reverse [a, b, c]

main :: IO()
main = do
    _ <- print "Hello world"
    r <- f <$> getChar <*> getChar <*> getChar
    _ <- putStrLn ""
    _ <- print "a" >> print "b" >> print "c"
    end <- putStrLn r
    return end



-- (<$>) :: Applicative f => (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

--(<$>) :: (a -> b -> c) -> f a -> f (b -> c)


-- (>>) :: Monad m => m a -> m b -> m b