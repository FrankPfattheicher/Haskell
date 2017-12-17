module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

roman :: Char -> Integer
roman 'I' = 1
roman 'V' = 5
roman 'X' = 10
roman 'L' = 50
roman 'C' = 100
roman 'D' = 500
roman 'M' = 1000

parseRoman :: String -> Integer
parseRoman "" = 0
parseRoman (s : r : rs) = 
    let vs = roman s in
    let vr = roman r in
    --case (vs < vr) of
    --    True -> vr - vs + (parseRoman rs)
    --    False -> vs + (parseRoman (r : rs))
    if (vs < vr)
    then vr - vs + (parseRoman rs)
    else vs + (parseRoman (r : rs))
        
--parseRoman (r : rs) = (roman r) + (parseRoman rs)

