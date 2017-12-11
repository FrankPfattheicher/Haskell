module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- chapter 4

-- Warum data / type


data Mood = 
      Blah 
    | Woot
    deriving Show

changeMood Blah = Woot
changeMood _ = Blah

type TestMood = Mood


-- not True && True
-- not (x == 6)
-- (1 * 2) > 5
-- "Merry" > "Happy"
-- ['1', '2', '3'] ++ "look at me!"

greetIfCool :: String -> IO ()
greetIfCool coolness =
    if cool then 
        putStrLn "eyyyyy. What's shakin'?"
    else
        putStrLn "pshhhh."
    where
        cool = coolness == "downright frosty yo"

greetIfCool2 :: String -> IO ()
greetIfCool2 coolness =
    if coolness == "downright frosty yo" then 
        putStrLn "eyyyyy. What's shakin'?"
    else
        putStrLn "pshhhh."


awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]


data Color a = 
      Black 
      | White 
      | Color
      | Gray a
      | RGB a a a

      

-- chapter 5

curry f a b = f (a, b)
