{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Substitution where

import Data.Char (toLower, toUpper)
import Data.Map (Map)
import qualified Data.Map as Map

import HabitOfFate.Unicode

data Gender = Male | Female | Neuter

data Entity = Entity String Gender

type Substitutions = Map String String

makeSubstitutionTable :: [(String,Entity)] → Substitutions
makeSubstitutionTable = Map.fromList . go True mempty
  where
    go :: Bool → [(String,String)] → [(String,Entity)] → [(String,String)]
    go _ table [] = table
    go first table ((key,(Entity name gender)):rest) =
        go False ((key,name) : pronoun_entries  ⊞  article_entries  ⊞  gender_entries  ⊞  table) rest
      where
        pronoun_entries
          | first = []
        article_entries =
            [("a " ⊞ key,article_value)
            ,("an " ⊞ key,article_value)
            ,("the " ⊞ key,"the " ⊞ article_value)
            ]
          where
            article
              | elem (toLower ∘ head $ name) ['a','e','i','o','u'] = "an"
              | otherwise = "a"
            article_value = article ⊞ " " ⊞ name

        subject = case gender of
            Male → "he"
            Female → "she"
            Neuter → "it"

        capitalized_subject = toUpper (head subject):tail subject

        object = case gender of
            Male → "him"
            Female → "her"
            Neuter → "it"

        category = case gender of
            Male → "man"
            Female → "woman"
            Neuter → "thing"

        default_gender_entries
          | first =
                [("He",capitalized_subject)
                ,("She",capitalized_subject)
                ,("It",capitalized_subject)
                ,("he",subject)
                ,("she",subject)
                ,("it",subject)
                ]
          | otherwise = []

        gender_entries =
            [("He|" ⊞ key,capitalized_subject)
            ,("She|" ⊞ key,capitalized_subject)
            ,("It|" ⊞ key,capitalized_subject)
            ,("he|" ⊞ key,subject)
            ,("she|" ⊞ key,subject)
            ,("it|" ⊞ key,subject)
            ,("him|" ⊞ key,object)
            ,("her|" ⊞ key,object)
            ,("man|" ⊞ key,category)
            ,("woman|" ⊞ key,category)
            ,("thing|" ⊞ key,category)
            ]

substitute :: Substitutions → String → String
substitute table = go
  where
    go ('{':rest) =
        let (possibly_wrapped_key,remainder) = break (== '}') rest
            key = unwords . words $ key
        in case Map.lookup key table of
            Nothing → error $ "key " ⊞ key ⊞ " was not found in the table"
            Just value → value ⊞ go (tail remainder)
    go (x:rest) = x:go rest
    go [] = []
