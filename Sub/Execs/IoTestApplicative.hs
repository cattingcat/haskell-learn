module Main where


-- main :: IO ()
-- main = do 
--     line <- getLine
--     let tmp = if line == "a" then pure (\x -> x + 1) else pure (\x -> x * 10)
--     r <- tmp <*> (pure 555)
--     putStrLn (show r)
--     return ()
    

{-
    main :: IO ()
    main = do 
        let lineFoo = fmap (\line -> if line == "a" then (\x -> x + 1) else (\x -> x * 10)) getLine
        r <- lineFoo <*> (pure 555)
        putStrLn (show r)
        return ()
-}

main :: IO ()
main = do 
    let lineFoo = (\line -> if line == "a" then (\x -> x + 1) else (\x -> x * 10)) <$> getLine
    r <- lineFoo <*> (pure 555)
    putStrLn (show r)
    return ()