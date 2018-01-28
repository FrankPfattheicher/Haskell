{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib
    ( someFunc
    ) where

import Data.Char 
import Data.Time
import Data.Int
import Data.List

import qualified Data.Text as T


someFunc :: IO ()
someFunc = putStrLn "someFunc"

-------------------------------------------------------
-- chapter 9 - Lists
-------------------------------------------------------

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


-- Chapter Exercises
-- Data.Char

-- 2
xupper :: String -> String
xupper s = [c | c <- s, isUpper c]

-- 3
capitalize :: String -> String
capitalize [c] = [toUpper c] 
capitalize (s:ss) = [toUpper s] ++ ss 

-- 4
capitalize2 :: String -> String
capitalize2 [c] = [toUpper c] 
capitalize2 (s:ss) = [toUpper s] ++ capitalize2 ss 

-- 5
capitalize3 :: String -> String
capitalize3 [c] = [toUpper c] 
capitalize3 s = [toUpper (head s)] ++ capitalize3 (tail s) 

-- 6
-- hä?

-- Writing your own standard functions

-- 1
myOr :: [Bool] -> Bool
myOr [x] = x
myOr (True:_) = True
myOr (False:xs) = myOr xs

-- 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f [x] = f x
myAny f (x:xs) = 
    if f x
    then True
    else myAny f xs

-- 3
myElem :: Eq a => a -> [a] -> Bool
myElem x [y] = x == y
myElem x (y:ys) = (x == y) || (myElem x ys)

-- 4
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = (myReverse xs) ++ [x]
-- myReverse "blah"
-- myReverse [1..5]

-- 5
squish :: [[a]] -> [a]
squish [] = []
squish [[x]] = [x]
squish (x:xs) = x ++ squish xs
-- squish ["abc","def"]
   
-- 6
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f [x] = f x
squishMap f (x:xs) = (f x) ++ squishMap f xs
-- squishMap (\x -> [1, x, 3]) [2]
-- squishMap (\x -> "WO "++[x]++" HOO ") "123"

-- 7
squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain (y:ys) = (squishMap (\x -> [x]) y) ++ squishAgain ys
-- squishAgain ["abc","def"]

-- 8
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [x] = x
myMaximumBy f (x:xs) = 
    let y = myMaximumBy f xs in
        if f x y == GT
        then x
        else y

-- myMaximumBy compare  [1, 53, 9001, 10]

-- 9
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f [x] = x
myMinimumBy f (x:xs) = 
    let y = myMinimumBy f xs in
        if f x y == LT
        then x
        else y

-- myMinimumBy compare  [1, 53, 9001, 10]

-- 10
myMaximum :: (Ord a) => [a] -> a
myMaximum [x] = x
myMaximum (x:xs) = 
    let y = myMaximum xs in
        if x > y
        then x
        else y

myMinimum :: (Ord a) => [a] -> a
myMinimum [x] = x
myMinimum (x:xs) = 
    let y = myMinimum xs in
        if x < y
        then x
        else y

-------------------------------------------------------
-- chapter 10 - Folding lists
-------------------------------------------------------

-- Exercises: Understanding Folds

-- foldl (flip (*)) 1 [1..3]
-- f' = flip (*)
-- (1 f' (2 f' (3 f' 1)))
-- (((1 * 3) * 2) * 1)


-- Exercises: Database Processing
data DatabaseItem = DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbNumber 19001
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]    

-- 1
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate (x:xs) = case x of
        DbDate dt -> [dt] ++ filterDbDate xs
        _         -> filterDbDate xs

-- filterDbDate2 xs = [x | DbDate x <- xs] 

-- 2
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber (x:xs) = case x of
    DbNumber n -> [n] ++ filterDbNumber xs
    _ -> filterDbNumber xs


-- 3
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = myMaximum (filterDbDate db)

-- 4
sumDb :: [DatabaseItem] -> Integer
sumDb db = sum (filterDbNumber db)

-- 5
avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral (sum ns)) / (fromIntegral (length ns))
    where ns = filterDbNumber db

-- Scans    
fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x    

-- 1
fibs20 = take 20 fibs

-- 2
fibs100 = takeWhile (< 100) fibs

-- 3
factR :: Integer -> Integer
factR 0 = 1
factR n = n * factR (n - 1)

factS :: Integer -> Integer
factS n = head $ reverse $ take (fromInteger n) (scanl (\x y -> x * y) 1 [2..])


-- Chapter Exercises

-- 1
stops = "pbtdkg"
vowels = "aeiou"

-- 1a
stop_vowel_stop :: String -> String -> [(Char,Char,Char)] 
stop_vowel_stop stop vowel = [(s1, v, s2) | s1 <- stop, v <- vowel, s2 <- stop]

-- 1b
stop_vowel_stop_p = filter (\(s1,_,_) -> s1 == 'p') $ stop_vowel_stop stops vowels

-- 1c
-- [Char] --> [String]


-- 2
seekritFunc x =
    div (sum (map length (words x)))
    (length (words x))
-- average word length

-- 3
seekritFunc2 x =
    let s = sum (map length (words x))
        n = length (words x) in
    (fromIntegral s) / (fromIntegral n)

-- Rewriting functions using folds

-- 1
myOr2 :: [Bool] -> Bool
myOr2 = foldr (||) False

-- 2
myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 f = foldr (\x y -> y || f x) False

-- 3
myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x xs = myAny (\a -> a == x) xs

myElem3 :: Eq a => a -> [a] -> Bool
myElem3 x xs = foldr (\a b -> b || a == x) False xs

-- 4
myReverse2 :: [a] -> [a]
myReverse2 xs = foldl (\b a -> [a] ++ b) [] xs

-- 5
myMap2 :: (a -> b) -> [a] -> [b]
myMap2 f xs = foldr (\a b -> [f a] ++ b) [] xs

-- 6
myFilter2 :: (a -> Bool) -> [a] -> [a]
myFilter2 f xs = foldr (\a b -> if f a then [a] ++ b else b) [] xs

-- 7
squish2 :: [[a]] -> [a]
squish2 xs = foldr (\a b -> a ++ b) [] xs

-- 8
squishMap2 :: (a -> [b]) -> [a] -> [b]
squishMap2 f xs = foldr (\a b -> f a ++ b) [] xs
-- squishMap2 (\x -> [1, x, 3]) [2]
-- squishMap2 (\x -> "WO " ++ [x] ++ " OT ") "blah"

-- 9
squishAgain2 :: [[a]] -> [a]
squishAgain2 xs = squishMap2 (\x -> x) xs

-- 10
myMaximumBy2 :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy2 f xs = foldl 
                        (\b a -> if (f b a) == GT then b else a) 
                        (head xs) xs
-- myMaximumBy2 (\_ _ -> GT) [1..10]
-- myMaximumBy2 (\_ _ -> LT) [1..10]
-- myMaximumBy2 compare [1..10]

-- 11
myMinimumBy2 :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy2 f xs = foldl 
                        (\b a -> if (f b a) == LT then b else a) 
                        (head xs) xs
-- myMinimumBy2 (\_ _ -> GT) [1..10]
-- myMinimumBy2 (\_ _ -> LT) [1..10]
-- myMinimumBy2 (\_ _ -> LT) [1..10]


-------------------------------------------------------
-- chapter 11 - Algebraic data types
-------------------------------------------------------

-- Exercises: Vehicles

data Price =
    Price Integer 
    deriving (Eq, Show)

data Manufacturer =
    Mini
    | Mazda
    | Tata
    deriving (Eq, Show)

data Airline =
    PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle = 
    Car Manufacturer Price
    | Plane Airline Integer
    deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir 30

-- 2
isCar :: Vehicle -> Bool
isCar c = case c of
    Car _ _ -> True
    _ -> False

isPlane :: Vehicle -> Bool
isPlane p = case p of
    Plane _ _ -> True
    _ -> False

areCars :: [Vehicle] -> [Bool]
areCars vs = map (\v -> isCar v) vs
-- areCars [myCar, urCar, clownCar, doge]

-- 3
getManu :: Vehicle -> Manufacturer
getManu v = case v of
    Car m _ -> m
    Plane _ _ -> undefined

-- 4 - Error: Non-exhaustive patterns in case

-- 5
-- Aufpassen: doge = Plane PapuAir 30


-- Exercises: Cardinality
-- 1 - 1
-- 2 - 3
-- 3 - 0x10000

-- Exercises: For Example

data Example = MakeExample deriving Show
data Example2 = MakeExample2 Int deriving Show

-- 1 - Example
-- 2 - Show
-- 3 - MakeExample2 :: Int -> Example2


-- newtype

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats =
    Goats Int deriving (Eq, Show)
newtype Cows =
    Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

instance TooMany Goats where
    tooMany (Goats n) = n > 43

-- Exercises: Logic Goats

-- 1
instance TooMany (Int, String) where
    tooMany (i, s) = i > 43

-- 2
instance TooMany (Int, Int) where
    tooMany (i, j) = (i + j) > 43

-- 3
instance (Num a, Ord a, TooMany a) => TooMany (a, a) where
    tooMany (i, j) = (i + j) > 43
-- * Could not deduce (Ord a) arising from a use of `>'
--   from the context: (Num a, TooMany a)
--   bound by the instance declaration at src\Lib.hs:517:10-45
-- Possible fix:
--   add (Ord a) to the context of the instance declaration
-- * In the expression: (i + j) > 43
--   In an equation for `tooMany': tooMany (i, j) = (i + j) > 43
--   In the instance declaration for `TooMany (a, a)'



-- Exercises: Pity the Bool

-- 1 - 4, i.e. (2 + 2)
data BigSmall =
    Big Bool
    | Small Bool
    deriving (Eq, Show)

-- 2 - 258, i.e. (256 + 2)
data NumberOrBool =
    Numba Int8
    | BoolyBool Bool
    deriving (Eq, Show)

myNumba = Numba (-128)


-- Record syntax

data Person =
    Person { pname :: String
           , page :: Int }
    deriving (Eq, Show)

jm = Person "julie" 108
ca = Person "chris" 16


-- Exercises: How Does Your Garden Grow?

-- 1 - "glaube" 4
data FlowerType = Gardenia
    | Daisy
    | Rose
    | Lilac
    deriving Show

type Gardener = String

data Garden =
    Garden Gardener FlowerType
    deriving Show


-- Constructing and deconstructing values

data GuessWhat =
    Chickenbutt deriving (Eq, Show)
    
data Id a =
    MkId a deriving (Eq, Show)

data Product a b =
    Product a b deriving (Eq, Show)

data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)

data RecordProduct a b =
    RecordProduct { pfirst :: a
                  , psecond :: b }
    deriving (Eq, Show)


newtype NumCow =
        NumCow Int
        deriving (Eq, Show)

newtype NumPig =
        NumPig Int
        deriving (Eq, Show)

data Farmhouse =
        Farmhouse NumCow NumPig
        deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig    


newtype NumSheep =
    NumSheep Int
    deriving (Eq, Show)
    
data BigFarmhouse =
    BigFarmhouse NumCow NumPig NumSheep
    deriving (Eq, Show)

type BigFarmhouse' =
    Product NumCow (Product NumPig NumSheep)


type Name = String
type Age = Int
type LovesMud = Bool


type PoundsOfWool = Int

data CowInfo =
    CowInfo Name Age
    deriving (Eq, Show)

data PigInfo =
    PigInfo Name Age LovesMud
    deriving (Eq, Show)

data SheepInfo =
    SheepInfo Name Age PoundsOfWool
    deriving (Eq, Show)

data Animal =
    Cow CowInfo
    | Pig PigInfo
    | Sheep SheepInfo
    deriving (Eq, Show)

type Animal' =
    Sum CowInfo (Sum PigInfo SheepInfo)


bess' = (CowInfo "Bess" 4)
bess = First bess' :: Animal'

e' = Second (SheepInfo "Elmer" 5 5)
elmer = Second e' :: Animal'


type Awesome = Bool

person :: Product Name Awesome
person = Product "Simon" True


data OperatingSystem =
    GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)
    
data ProgLang =
    Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
               , lang :: ProgLang }
    deriving (Eq, Show)

    
nineToFive :: Programmer
nineToFive = Programmer { os = Mac
                        , lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda
                             , os = GnuPlusLinux }

-- Exercise: Programmers
allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgLang]
allLanguages =
    [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer o l | o <- allOperatingSystems, l <- allLanguages]


-- Deconstructing values

newtype Acres = Acres Int deriving Show

-- FarmerType is a Sum
data FarmerType = DairyFarmer
    | WheatFarmer
    | SoybeanFarmer
    deriving (Show, Eq)

-- Farmer is a plain ole product of
-- Name, Acres, and FarmerType
data Farmer =
    Farmer Name Acres FarmerType
    deriving Show


isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

isDairyFarmer2 :: Farmer -> Bool
isDairyFarmer2 (Farmer _ _ ty) = ty == DairyFarmer
        

-- ?????????????????????? see Person { name :: Name }

data FarmerRec =
    FarmerRec { name :: Name
              , acres :: Acres
              , farmerType :: FarmerType }
    deriving Show
    
isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
    case farmerType farmer of
        DairyFarmer -> True
        _ -> False


-- Exercises: The Quad

-- 1
data Quad =
    One
    | Two
    | Three
    | Four
    deriving (Eq, Show, Enum, Bounded)

-- how many different forms can this take?
eQuad :: Either Quad Quad
eQuad = Left One
-- ...
-- eQuad = Left Four
-- eQuad = Right One
-- ...
-- eQuad = Right Four
-- ===>  4

-- 2 - 16, i.e. (4 * 4)

-- 3 - 16, i.e. (4 * 4)

-- 4 - 8, i.e. (2 * 2 * 2)

-- 5 - 8, i.e. (2 * 2 * 2)

-- 6 - 32, i.e. (2 * 4 * 4) or 0b10_00_00


-- Binary Tree

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a
                    -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)    

t1 = insert' 10 Leaf
t2 = insert' 13 t1    
t3 = insert' 15 t2    
t4 = insert' 5 t3   


mapTree :: (a -> b) -> BinaryTree a
           -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
    Node (mapTree f left) (f a) (mapTree f right)


testTree :: BinaryTree Integer
testTree =
    Node (Node Leaf 1 Leaf)
    2
    (Node Leaf 3 Leaf)    

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) =
    [a] ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) =
    (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) =
    (postorder right) ++ [a] ++ (postorder left)


foldTree :: (a -> b -> b) -> b -> BinaryTree a
            -> b
foldTree f b Leaf = b
foldTree f b (Node left a right) =
    f a (foldTree f (foldTree f b right) left)


-- Chapter Exercises

data Weekday =
    Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday


-- Ciphers
keyword = "ALLY"
keyword_len = length keyword

cipher_char_shift :: Char -> Int
cipher_char_shift a = ord a - ord 'A'

test_text = "MEET AT DAWN"
test_ciphered = "MPPR AE OYWY"

vigenère_kw :: Int -> String -> String
vigenère_kw _ "" = ""
vigenère_kw ix (c:cs) = 
    let r = keyword !! (rem ix keyword_len) in
        case c of
            ' ' -> ' ' : (vigenère_kw ix cs)
            _ -> r : (vigenère_kw (ix + 1) cs)

cipher :: Int -> Char -> Char
cipher _ ' ' = ' '
cipher x c = 
    let code = ord c
        index = code - ord 'A'
        codedix = rem (index + x) 26
        coded = codedix + ord 'A' in
    chr coded

                        
vigenère :: String -> String
vigenère txt =
    let m = vigenère_kw 0 txt
        o = map (\c -> if c == ' ' then 0 else ord c - ord 'A') m 
        z = zip txt o in
    map (\(c, o) -> cipher o c) z



-- As-patterns

asPattern :: String -> (Char, String, String)
asPattern txt@(c:cs) = (c, cs, txt)

-- 1
isSubseqOf :: (Eq a) => [a] -> [a]
              -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf sx@(s:ss) (t:ts) =
    ((s == t) && (isSubseqOf ss ts)) || (isSubseqOf sx ts)

-- isSubseqOf "blah" "blahwoot"
-- isSubseqOf "blah" "wootblah"
-- isSubseqOf "blah" "wboloath"
-- isSubseqOf "blah" "wootbla"
-- isSubseqOf "blah" "halbwoot"
-- isSubseqOf "blah" "xxblawhoot"
-- isSubseqOf "blah" "bla"

-- Language exercises
-- 1
capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (c:cs) = (toUpper c : cs)

-- 2
capitalizeSentence :: String -> String
capitalizeSentence "" = ""
capitalizeSentence (' ':cs) = " " ++ capitalizeSentence cs
capitalizeSentence txt@(_:_) = capitalizeWord txt

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph txt = 
    let sentences = (mySplit '.' txt) in
        concat $ map (\s -> (capitalizeSentence s) ++ ".") sentences

-- capitalizeParagraph "blah. woot ha."


-- Phone exercise

-- 1
data DaPhone = DaPhone [String] deriving Show
        
phone_layout = DaPhone
        [
            "1",
            "ABC2",
            "DEF3",
            "GHI4",
            "JKL5",
            "MNO6",
            "PQRS7",
            "TUV8",
            "WXYZ9",
            "+ 0"
        ]


-- 2
convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

reverseTaps :: DaPhone -> Char
               -> [(Digit, Presses)]
reverseTaps (DaPhone pl) c = 
    let ch = toUpper c
        shift = if isUpper c then [('*',1)] else []
        key = find (\x -> elem ch x) pl in
        case key of
            Nothing -> []
            Just k -> let p = elemIndex ch k in
                case p of
                    Nothing -> []
                    Just pos -> shift ++ [((last k), pos + 1)]

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
cellPhonesDead :: DaPhone -> String
                -> [(Digit, Presses)]
cellPhonesDead _ "" = []
cellPhonesDead pl (c:cs) = (reverseTaps pl c) ++ cellPhonesDead pl cs
-- cellPhonesDead phone_layout "Test"

-- 3
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps keys = foldr (\(d,p) b -> p + b) 0 keys


-- 4
mostPopularLetter :: String -> Char
mostPopularLetter text = 
    let letters = groupBy (==) $ sortBy (\c1 c2 -> compare c2 c1) text
        popular = sortBy (\s1 s2 -> compare (length s2) (length s1)) letters in
        head $ head $ popular

mostPopularLetterCost :: String -> Int
mostPopularLetterCost text = 
    let popular = mostPopularLetter text 
        count = foldr (\a b -> if a == popular then b + 1 else b) 0 text
        presses = cellPhonesDead phone_layout [popular] in
        count * length presses


-- 5
coolestLtr :: [String] -> Char
coolestLtr text = 
    let letters = groupBy (==) $ sortBy compare $ map mostPopularLetter text
        popular = sortBy (\s1 s2 -> compare (length s2) (length s1)) letters in
        head $ head popular

coolestWord :: [String] -> String
coolestWord text = 
    let words = squish $  map myWords text
        count = groupBy (==) $ sortBy (\s1 s2 -> compare (length s2) (length s1)) words 
        popular = sortBy (\s1 s2 -> compare (length s2) (length s1)) count in
        head $ head  popular


-- Hutton’s Razor

-- 1
data Expr
    = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval expr = 
    case expr of
        Lit i -> i
        Add e1 e2 -> (eval e1) + (eval e2)

-- eval (Add (Lit 1) (Lit 9001))

-- 2
printExpr :: Expr -> String
printExpr expr =  
    case expr of
        Lit i -> show i
        Add e1 e2 -> (printExpr e1) ++ " + " ++ (printExpr e2)

-- printExpr (Add (Lit 1) (Lit 9001))
a1 = Add (Lit 9001) (Lit 1)
a2 = Add a1 (Lit 20001)
a3 = Add (Lit 1) a2
-- printExpr a3


-------------------------------------------------------
-- chapter 12 - Signaling adversity
-------------------------------------------------------

-- 1 - *
xid :: a -> a
xid a = a

-- 2 - ?????????????? f
-- rrr :: a -> f a
-- rrr a = a


-- String processing

-- 1
notThe :: String -> Maybe String
notThe the = case the of
    "the" -> Nothing
    _ -> Just the

replaceThe :: String -> String
replaceThe text = 
    let words = myWords text
        nothe = map notThe words
        toa = map (\m -> case m of { Just w -> w; _ -> "a"}) nothe
    in
        foldr (\w s -> w ++ " " ++ s) "" toa


-- 2
isVowel :: Char -> Bool
isVowel c = elem c "aeiouAEIOU"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel text =
    let words = myWords text
        pairs = zip words $ drop 1 words
        thevo = map (\(t,w) -> (t == "the") && isVowel (head w)) pairs
    in  
        foldr (\a b -> if a then b + 1 else b) 0 thevo

-- countTheBeforeVowel "the evil cow and the old cat"


-- 3
countVowels :: String -> Integer
countVowels text = 
    toInteger $ length $ foldr (\a b -> b ++ if isVowel a then [a] else []) "" text

-- countVowels "the cow"
-- countVowels "Mikolajczak"


-- Validate the word

newtype Word' = 
    Word' String
    deriving (Eq, Show)


mkWord :: String -> Maybe Word'
mkWord text = 
    let vovels = fromIntegral $ countVowels text
        consonants = (length text) - vovels
    in
        if vovels > consonants
        then Nothing
        else Just (Word' text)


-- It’s only Natural

data Nat =
    Zero
    | Succ Nat
    deriving (Eq, Show)


natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x
        
-- natToInteger Zero
-- 0
-- natToInteger (Succ Zero)
-- 1
-- natToInteger (Succ (Succ Zero))
-- 2

integerToNat :: Integer -> Maybe Nat
integerToNat n = 
    case compare n 0 of
        LT -> Nothing
        EQ -> Just Zero
        GT -> case integerToNat (n-1) of
                Just Zero -> Just (Succ Zero)
                Just x -> Just (Succ x)

-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing


-- Small library for Maybe

-- 1

-- >>> isJust (Just 1)
-- True
-- >>> isJust Nothing
-- False
isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust _ = False

-- >>> isNothing (Just 1)
-- False
-- >>> isNothing Nothing
-- True
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False


-- 2

-- >>> mayybee 0 (+1) Nothing
-- 0
-- >>> mayybee 0 (+1) (Just 1)
-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f Nothing = b
mayybee b f (Just a) = f a


-- 3

-- >>> fromMaybe 0 Nothing
-- 0
-- >>> fromMaybe 0 (Just 1)
-- 1
fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just v) = v


-- 4

-- >>> listToMaybe [1, 2, 3]
-- Just 1
-- >>> listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]


-- 5
-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- >>> let xs = take 3 $ repeat Nothing
-- >>> catMaybes xs
-- []
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just x):xs) = x : catMaybes xs


-- 6

-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing:xs) = Nothing
flipMaybe ((Just x):xs) = 
    case flipMaybe xs of
        Nothing -> Nothing
        Just xx -> Just (x : xx)



-- Small library for Either

-- 1
lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' (x:xs) = 
    case x of
        Left a -> [a] ++ lefts' xs
        Right _ -> lefts' xs

-- 2
rights' :: [Either a b] -> [b]
rights' list = foldr (\a b -> case a of {Right a -> [a] ++ b; Left _ -> b}) [] list


-- 3
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' list = (lefts' list, rights' list)

-- partitionEithers' [Left 2, Right 3, Left 4]


-- 4
eitherMaybe' :: (b -> c) -> Either a b
            -> Maybe c
eitherMaybe' f e = 
    case e of
        Left l -> Nothing
        Right r -> Just $ f r


-- 5
either' :: (a -> c)
    -> (b -> c)
    -> Either a b
    -> c

either' fa fb e = 
    case e of
        Left l -> fa l
        Right r -> fb r
-- either' (+1) (+2) (Left 5)
-- 6
-- either' (+1) (+2) (Right 5)
-- 7    


-- 6
eitherMaybe'' :: (b -> c) -> Either a b
        -> Maybe c
eitherMaybe'' f e = either' (\a -> Nothing) (\b -> Just (f b)) e 
    


-- Write your own iterate and unfoldr

-- 1
myIterate :: (a -> a) -> a -> [a]
myIterate f a = [a] ++ myIterate f (f a)

-- 2
myUnfoldr :: (b -> Maybe (a, b)) -> b
          -> [a]
myUnfoldr f b = 
    case f b of
        Nothing -> []
        Just (x,y) -> [x] ++ myUnfoldr f y

-- take 10 $ myUnfoldr (\b -> Just (b, b+1)) 0
-- [0,1,2,3,4,5,6,7,8,9]


-- 3
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x

-- take 10 $ betterIterate (+1) 0
-- [0,1,2,3,4,5,6,7,8,9]



-- Finally something other than a list!

-- 1
unfold :: (a -> Maybe (a,b,a)) -> a
          -> BinaryTree b
unfold f a = 
    case f a of
        Nothing -> Leaf
        Just (x1,y,x2) -> Node (unfold f x1) y (unfold f x2)


-- unfold (\x -> if x > 0 then Just (x-1,x,x-1) else Nothing) 5



-- 2
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x < n then Just (x+1,x,x+1) else Nothing) 0


-- treeBuild 0
-- Leaf

-- treeBuild 1
-- Node Leaf 0 Leaf

-- treeBuild 2
-- Node (Node Leaf 1 Leaf)
--      0
--      (Node Leaf 1 Leaf)

-- treeBuild 3
-- Node (Node (Node Leaf 2 Leaf)
--            1
--            (Node Leaf 2 Leaf))
--      0
-- (Node (Node Leaf 2 Leaf)
--             1
--             (Node Leaf 2 Leaf))


