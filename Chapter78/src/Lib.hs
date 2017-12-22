module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

test x 
    | x < 5 = 8
    | otherwise = 0


f x = 
    let y = test x
    in
        x + y

