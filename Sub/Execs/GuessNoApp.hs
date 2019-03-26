module Main where

import Text.Read

data GuessRes = GuessQuit | GuessSuccess
data GuessAttemt = Quit | NumberAttempt Int | Incorrect

instance Read GuessAttemt where 
    readsPrec i s 
        | s == "q" || s == "Q" = [(Quit, "")]
        | otherwise = [(res, "")]
        where   res = case mi of 
                    Just n -> NumberAttempt n
                    _ -> Incorrect
                    where mi = readMaybe s

main :: IO ()
main = do 
    putStrLn "Welcone to guess number game"
    loop

loop = do
    putStrLn "Lets start! Type number limit or q to exit"
    line <- getLine
    if line == "q" then return () else do
        putStrLn "Ok, lets try"
        r <- guessingLoop 555
        case r of
            GuessQuit -> return ()
            GuessSuccess -> loop


guessingLoop :: Int -> IO GuessRes
guessingLoop n = do
    line <- getLine
    let att = read line :: GuessAttemt
    case att of
        NumberAttempt num -> processNum num 
        Incorrect -> do 
            putStrLn "Incorrect number :("
            guessingLoop n
        Quit -> return GuessQuit
        where processNum num  
                | num == n = do
                    putStrLn "Success :)"
                    return GuessSuccess
                | num > n = do 
                    putStrLn "Greater :("
                    guessingLoop n
                | otherwise = do 
                    putStrLn "Less :("
                    guessingLoop n
    
