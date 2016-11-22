{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Substitution where

import Control.Lens ((<>~), _1, makeLenses)
import Data.Char (toLower, toUpper)
import Data.Map (Map)
import qualified Data.Map as Map

import HabitOfFate.TH
import HabitOfFate.Unicode

data Gender = Male | Female | Neuter deriving (Eq,Ord,Read,Show)
deriveJSON ''Gender

data Character = Character String Gender deriving (Eq,Ord,Read,Show)
deriveJSON ''Character

type Substitutions = Map String String

makeSubstitutionTable ∷ [(String,Character)] → Substitutions
makeSubstitutionTable [] = Map.empty
makeSubstitutionTable table@((_,first_character@(Character name _)):_) =
    Map.fromList
    $
    makeNouns first_character
    ⊕
    concatMap
      (\(key, character@(Character name _)) →
          (name, name) : makeArticles key character ⊞ map (_1 <>~ '|':key) (makeNouns character)
      )
      table
  where
    makeArticles key (Character name _) =
        [("a " ⊞ key,article_value)
        ,("A " ⊞ key,article_value)
        ,("an " ⊞ key,article_value)
        ,("An " ⊞ key,article_value)
        ,("the " ⊞ key,"the " ⊞ name)
        ,("The " ⊞ key,"The " ⊞ name)
        ]
      where
        article_value = article ⊞ " " ⊞ name
          where
            article
              | elem (toLower ∘ head $ name) ['a','e','i','o','u'] = "an"
              | otherwise = "a"

    makeNouns (Character name gender) = concat
        [subject_pronouns
        ,object_pronouns
        ,possessive_prononuns
        ,descriptive_possessive_pronouns
        ,category_nouns
        ]
      where
        capitalized word = toUpper (head word):tail word

        subject_pronouns =
            map (,pronoun) ["he","she","it"]
            ⊞
            map (,capitalized pronoun) ["He","She","It"]
          where
            pronoun = case gender of
                Male → "he"
                Female → "she"
                Neuter → "it"

        object_pronouns = map (,pronoun) ["him","her","it"]
          where
            pronoun = case gender of
                Male → "him"
                Female → "her"
                Neuter → "it"

        possessive_prononuns =
            map (,pronoun) ["his","her'","its"]
            ⊞
            map (,capitalized pronoun) ["His","Her","Its"]
          where
            pronoun = case gender of
                Male → "his"
                Female → "her"
                Neuter → "its"

        descriptive_possessive_pronouns =
            map (,pronoun) ["his","hers","its"]
          where
            pronoun = case gender of
                Male → "his"
                Female → "her"
                Neuter → "its"

        category_nouns =
            map (,category) ["man","woman","thing"]
          where
            category = case gender of
                Male → "man"
                Female → "woman"
                Neuter → "thing"

substitute ∷ Substitutions → String → String
substitute table = go
  where
    go ('{':rest) =
        let (possibly_wrapped_key,remainder) = break (== '}') rest
            key = unwords . words $ possibly_wrapped_key
        in case Map.lookup key table of
            Nothing → error $ "key " ⊞ key ⊞ " was not found in the table"
            Just value → value ⊞ go (tail remainder)
    go (x:rest) = x:go rest
    go [] = []
