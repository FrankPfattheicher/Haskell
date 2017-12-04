module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- chapter 2

-- p.45
--       class
--       constraint   in   out
triple :: Num a =>    a -> a
triple x = x * 3

--z =7
--y =z+8
--x =y^2
--waxOn=x*5

waxOn= x*5 where
    z =7
    y =z+8
    x =y^2


-- chapter 3

-- p.

