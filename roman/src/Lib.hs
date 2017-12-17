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

fromRoman :: String -> Integer
fromRoman "" = 0
fromRoman (s : r : rs) = 
    let vs = roman s in
    let vr = roman r in
    --case (vs < vr) of
    --    True -> vr - vs + (fromRoman rs)
    --    False -> vs + (fromRoman (r : rs))
    if (vs < vr)
    then vr - vs + (fromRoman rs)
    else vs + (fromRoman (r : rs))
        
fromRoman (r : rs) = (roman r) + (fromRoman rs)


toRoman :: Integer -> String
toRoman 0 = ""
toRoman n = 
    let y = [x | x <- "MDCLXVI", n >= (roman x)] in
    let z = y !! 0 in
    let vz = roman(z) in
        (z : toRoman (n - vz))
        
