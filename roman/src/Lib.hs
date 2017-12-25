module Lib
    ( someFunc
    ) where

--import Data.String.Utils
import Data.List
import Text.Show.Unicode

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Version 1

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
        
-- Version 2

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
        

-- Version 3 - including error handling

roman3 = reverse [
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

fromRoman3 :: String -> Maybe Integer
fromRoman3 "" = Just 0
fromRoman3 s =
    case find (\(r,n) -> isPrefixOf r s) roman3 of
        Just (r1,v1) -> case fromRoman3 (drop (length r1) s) of
            Just v2 -> Just (v1 + v2)
            Nothing -> Nothing    
        Nothing -> Nothing

toRoman3 :: Integer -> String
toRoman3 0 = ""
toRoman3 v = 
    case find (\(r,n) -> v >= n) roman3 of
        Just (r1,v1) -> r1 ++ toRoman3 (v - v1)
        Nothing -> "<negative>"
        
