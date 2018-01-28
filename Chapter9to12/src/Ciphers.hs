
module Ciphers 
    ( cipher
    ) where

import Data.Char

cipher :: Int -> Char -> Char
cipher x c = 
    let code = ord c
        index = code - ord 'a'
        codedix = rem (index + x) 26
        coded = codedix + ord 'a' in
    chr coded


caesarX :: Int -> String -> String
caesarX x [c] = [cipher x c]
caesarX x (c:cs) = [cipher x c] ++ caesarX x cs

caesar :: String -> String
caesar s = caesarX 5 s
