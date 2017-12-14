module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- chapter 6

data Trivial = Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True

data DayOfWeek = Mon| Tue| Weds| Thu| Fri| Sat| Sun
    deriving (Bounded, Enum, Ord, Show)

instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Weds Weds = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

-- day of week and numerical day of month
data Date = Date DayOfWeek Int

instance Eq Date where
    (==) (Date weekday1 dayOfMonth1)
         (Date weekday2 dayOfMonth2) = 
            weekday1 == weekday2 
            && dayOfMonth1 == dayOfMonth2


data Identity a = Identity a 
instance Eq a => Eq(Identity a) where
    (==) (Identity v) (Identity v') = v == v'

--Exercise 1
data TisAnInteger =
    TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn v) (TisAn v') = v == v'
    
--Exercise 2
data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two a b) (Two a' b') = (a == a') && (b == b')

--Exercise 3
data StringOrInt =
    TisAnInt Int
    | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt a') = (a == a')
    (==) (TisAString s) (TisAString s') = (s == s')
    (==) _ _ = False
    

newtype DayOfWeek2 = D2 DayOfWeek
    deriving (Show)

instance Eq DayOfWeek2 where
    (==) (D2 a) (D2 b) = a == b
    
instance Ord DayOfWeek2 where
    compare (D2 Fri) (D2 Fri) = EQ
    compare (D2 Fri) (D2 _) = GT
    compare (D2 _) (D2 Fri) = LT
    compare (D2 _) (D2 _) = EQ
    
data Person = Person Bool deriving Show

printPerson :: (Show person) => person -> IO ()
printPerson person = putStrLn (show person)


data Mood = Blah
    | Woot deriving (Show, Eq)

settleDown x = if x == Woot
    then Blah
    else x
    


i :: Num a => a
--i :: a
i = 1

--Fragen

-- data vs. newtype

