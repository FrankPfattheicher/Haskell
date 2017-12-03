module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- chapter 2

--z =7
--y =z+8
--x =y^2
--waxOn=x*5

waxOn= x*5 where
    z =7
    y =z+8
    x =y^2

-- chapter 5


-- chapter 6

data Trivial = Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True

data DayOfWeek = Mon| Tue| Weds| Thu| Fri| Sat| Sun

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

