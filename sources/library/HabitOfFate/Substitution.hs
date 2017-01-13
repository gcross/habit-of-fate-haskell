{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Substitution where

import Control.Applicative hiding (many)
import Control.Lens hiding (noneOf)
import Control.Monad.Writer.Strict
import Data.Char (toLower, toUpper)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy.Builder
import Text.Printf
import Text.Parsec
import Text.Parsec.Char

import HabitOfFate.TH
import HabitOfFate.Unicode

data Gender = Male | Female | Neuter deriving (Eq,Ord,Read,Show)
deriveJSON ''Gender

data Character = Character Text Gender deriving (Eq,Ord,Read,Show)
deriveJSON ''Character

type Substitutions = Map Text Text

makeSubstitutionTable ∷ [(Text,Character)] → Substitutions
makeSubstitutionTable [] = Map.empty
makeSubstitutionTable table@((_,first_character@(Character _ _)):_) =
    Map.fromList
    $
    makeNouns first_character
    ⊕
    concatMap
      (\(key, character@(Character name _)) →
          (name, name)
          :
          makeArticles key character ⊕ map (_1 ⊕~ Text.cons '|' key) (makeNouns character)
      )
      table
  where
    makeArticles ∷ Text → Character → [(Text,Text)]
    makeArticles key (Character name _) =
        [("a " ⊕ key,article_value)
        ,("A " ⊕ key,article_value)
        ,("an " ⊕ key,article_value)
        ,("An " ⊕ key,article_value)
        ,("the " ⊕ key,"the " ⊕ name)
        ,("The " ⊕ key,"The " ⊕ name)
        ]
      where
        article_value = article ⊕ " " ⊕ name
          where
            article
              | flip elem ("aeiou" ∷ String) ∘ toLower ∘ Text.head $ name = "an"
              | otherwise = "a"

    makeNouns ∷ Character → [(Text,Text)]
    makeNouns (Character _ gender) = concat
        [subject_pronouns
        ,object_pronouns
        ,possessive_prononuns
        ,descriptive_possessive_pronouns
        ,category_nouns
        ]
      where
        capitalized word = Text.cons (toUpper $ Text.head word) (Text.tail word)

        subject_pronouns =
            map (,pronoun) ["he","she","it"]
            ⊕
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
            ⊕
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


substitute ∷ Substitutions → Text → Text
substitute table =
  either
    (error ∘ show)
    id
  ∘
  runParser parser () ""
  where
    parser ∷ Parsec Text () Text
    parser =
      mappend
        <$> takeTillNextSub
        <*> (fmap mconcat ∘ many $ mappend <$> parseAnotherSub <*> takeTillNextSub)

    takeTillNextSub = fmap Text.pack ∘ many $ satisfy (/='{')

    parseAnotherSub = do
      char '{'
      key ← unwords ∘ words <$> many1 (satisfy (/='}'))
      when ('{' ∈ key) $ fail "nested brace"
      char '}'
      case Map.lookup (Text.pack key) table of
        Nothing → fail $ "key " ⊕ key ⊕ " was not found in the table"
        Just value → return value
