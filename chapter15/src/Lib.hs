module Lib
    ( someFunc
    ) where

import Data.Monoid

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Optional a =
      Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend Nada (Only o) = Only o    
    mappend (Only o) Nada = Only o    
    mappend (Only o1) (Only o2) = Only (o1 `mappend` o2)

-- Only (Sum 1) `mappend` Only (Sum 1)
-- Only (Product 4) `mappend` Only (Product 2)
-- Only (Sum 1) `mappend` Nada
-- Only [1] `mappend` Nada
-- Nada `mappend` Only (Sum 1)

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
    -> Adverb
    -> Noun
    -> Adjective
    -> String
madlibbin' e adv noun adj =
    e <> "! he said " <>
    adv <> " as he jumped into his car " <>
    noun <> " and drove off with his " <>
    adj <> " wife."

madlibbinBetter' :: Exclamation
    -> Adverb
    -> Noun
    -> Adjective
    -> String
madlibbinBetter' e adv noun adj = 
    mconcat [e, "! he said ", adv, " as he jumped into his car ", noun, " and drove off with his ", adj, " wife."]

-- madlibbin' "hey" "proudly" "hill" "silly"