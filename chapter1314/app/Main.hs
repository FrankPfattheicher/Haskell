module Main where

import Lib

main :: IO ()
main = do
    name <- getLine 
    sayHello name
    
