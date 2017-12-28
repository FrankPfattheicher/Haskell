module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- chapter 9 - Lists

eftBool :: Bool -> Bool -> [Bool]
eftBool min max = [min..max]

eftOrd :: Ordering
    -> Ordering
    -> [Ordering]
eftOrd min max = [min..max]

eftInt :: Int -> Int -> [Int]
eftInt min max = [min..max]

eftChar :: Char -> Char -> [Char]
eftChar min max = [min..max]


-- Exercises: Thy Fearful Symmetry
-- 1

myWords :: String -> [String]
myWords "" = [] 
myWords s = 
    let w = takeWhile (/= ' ') s
        d = 1 + length w in
    [w] ++ myWords (drop d s)

-- 2
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
            \ symmetry?"
sentences = firstSen ++ secondSen
    ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines "" = [] 
myLines s = 
    let w = takeWhile (/= '\n') s
        d = 1 + length w in
    [w] ++ myLines (drop d s)

shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]

-- 3
mySplit :: Char -> String -> [String]
mySplit ch "" = [] 
mySplit ch s = 
    let w = takeWhile (/= ch) s
        d = 1 + length w in
    -- [w] ++ mySplit ch (drop d s)
    w : mySplit ch (drop d s)

-- Exercises: Comprehend Thy Lists

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

acro s = [x | x <- s,  elem x ['A'..'Z']]

-- 1
mySqrCube1 = [(x,y) | x <- mySqr, y <- myCube]

-- 2
mySqrCube2 = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]


-- Exercises: Filtering

-- 1
f1 = [ x | x <- [1..30], rem x 3 == 0]

-- 2
f2 = length [ x | x <- [1..30], rem x 3 == 0]

-- 3
articles = ["the", "a", "an"]

myFilter :: String -> [String]
myFilter s = [w | w <- myWords s, not (elem w articles)]


-- Zipping exercises

-- 1
zip1 :: [a] -> [b] -> [(a, b)]
zip1 [] _ = []
zip1 _ [] = [] 
zip1 (x:xs) (y:ys) =
    (x,y) : zip1 xs ys

len :: [a] -> Int
len [] = 0
len (a:as) = 1 + len as

-- 2
zip2 :: (a -> b-> c) -> [a] -> [b] -> [c]
zip2 f [] _ = []
zip2 f _ [] = [] 
zip2 f (x:xs) (y:ys) =
    (f x y) : zip2 f xs ys

-- 3
zip03 :: [a] -> [b] -> [(a, b)]
zip03 as bs = zip2 (\a b -> (a,b)) as bs

-- chapter 10 - Folding lists



-- chapter 11 - Algebraic data types



-- chapter 12 - Signaling adversity


