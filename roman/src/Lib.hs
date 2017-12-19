module Lib
    ( someFunc
    ) where

--import Data.String.Utils
import Data.List
import Text.Show.Unicode

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

roman2 = reverse [
    ("I", 1),
    ("IV", 4),
    ("V", 5),
    ("IX", 9),
    ("X", 10),
    ("XL", 40),
    ("L", 50),
    ("XC", 90),
    ("C", 100),
    ("CD", 400),
    ("D", 500),
    ("CM", 900),
    ("M", 1000),
    ("ↁ", 5000),
    ("ↂ", 10000)
    ]

fromRoman2 :: String -> Integer
fromRoman2 "" = 0
fromRoman2 s =
    let [(r1, v1)] = take 1 [(r,n) | (r,n) <- roman2, isPrefixOf r s] in
        v1 + fromRoman2 (drop (length r1) s)

toRoman2 :: Integer -> String
toRoman2 0 = ""
toRoman2 v = 
    let [(r1, v1)] = take 1 [(r,n) | (r,n) <- roman2, v >= n ] in
        r1 ++ toRoman2 (v - v1)
        

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
        
