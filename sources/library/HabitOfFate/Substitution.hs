{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Substitution where

import HabitOfFate.Prelude hiding (many)

import qualified Data.Char as Char
import Data.Text.Lazy.Builder
import Text.Parsec
import Text.Parsec.Char

import HabitOfFate.TH

data Gender = Male | Female | Neuter deriving (Eq,Ord,Read,Show)
deriveJSON ''Gender

data Character = Character Text Gender deriving (Eq,Ord,Read,Show)
deriveJSON ''Character

type Substitutions = Map Text Text

makeSubstitutionTable ∷ [(Text,Character)] → Substitutions
makeSubstitutionTable [] = mempty
makeSubstitutionTable table@((_,first_character@(Character _ _)):_) =
    mapFromList
    $
    makeNouns first_character
    ⊕
    concatMap
      (\(key, character@(Character name _)) →
          (name, name)
          :
          makeArticles key character ⊕ fmap (_1 ⊕~ ('|' <| key)) (makeNouns character)
      )
      table
  where
    makeArticles ∷ Text → Character → [(Text,Text)]
    makeArticles key (Character name _) =
        [("a " ⊕ key, articleValue False)
        ,("A " ⊕ key, articleValue True)
        ,("an " ⊕ key, articleValue False)
        ,("An " ⊕ key, articleValue True)
        ,("the " ⊕ key, "the " ⊕ name)
        ,("The " ⊕ key, "The " ⊕ name)
        ]
      where
        articleValue ∷ 𝔹 → Text
        articleValue capitalize = article ⊕ " " ⊕ name
          where
            article =
              (_head %~ if capitalize then Char.toUpper else Char.toLower)
              $
              case name ^? _head of
                Just c | Char.toLower c ∈ "aeiou" → "an"
                _ → "a"

    makeNouns ∷ Character → [(Text,Text)]
    makeNouns (Character _ gender) = concat
        [subject_pronouns
        ,object_pronouns
        ,possessive_prononuns
        ,descriptive_possessive_pronouns
        ,category_nouns
        ]
      where
        capitalized = (_head %~ Char.toUpper)

        subject_pronouns =
            fmap (,pronoun) ["he","she","it"]
            ⊕
            fmap (,capitalized pronoun) ["He","She","It"]
          where
            pronoun = case gender of
                Male → "he"
                Female → "she"
                Neuter → "it"

        object_pronouns = fmap (,pronoun) ["him","her","it"]
          where
            pronoun = case gender of
                Male → "him"
                Female → "her"
                Neuter → "it"

        possessive_prononuns =
            fmap (,pronoun) ["his","her'","its"]
            ⊕
            fmap (,capitalized pronoun) ["His","Her","Its"]
          where
            pronoun = case gender of
                Male → "his"
                Female → "her"
                Neuter → "its"

        descriptive_possessive_pronouns =
            fmap (,pronoun) ["his","hers","its"]
          where
            pronoun = case gender of
                Male → "his"
                Female → "her"
                Neuter → "its"

        category_nouns =
            fmap (,category) ["man","woman","thing"]
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

    takeTillNextSub = (^. packed) <$> many (satisfy (/='{'))

    parseAnotherSub = do
      char '{'
      key ← unwords ∘ words <$> many1 (satisfy (/='}'))
      when ('{' ∈ key) $ fail "nested brace"
      char '}'
      case lookup (key ^. packed) table of
        Nothing → fail $ printf "key %s was not found in the table" key
        Just value → return value
