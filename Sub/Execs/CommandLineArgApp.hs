module Main where

import System.Environment
       

main = do
    args <- getArgs
    end <- putStrLn $ show args
    return end