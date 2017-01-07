{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Substitution where

import Control.Applicative
import Control.Lens
import Control.Monad.Writer.Strict
import Data.Attoparsec.Text
import Data.Char (toLower, toUpper)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy.Builder
import Text.Printf

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
substitute table t =
  either
    (error ∘ ("Error when performing substitutions: " ⊕))
    ((^. strict) ∘ toLazyText ∘ snd)
  ∘
  flip parseOnly t
  ∘
  runWriterT
  $
  takeTillNextSub >> parseAnotherSub
  where
    takeTillNextSub ∷ WriterT Builder Parser ()
    takeTillNextSub = (lift $ takeTill (== '{')) >>= tell ∘ fromText

    parseAnotherSub ∷ WriterT Builder Parser ()
    parseAnotherSub =
      (do
        key ← lift $ do
          _ ← char '{'
          key ← takeTill (== '}')
          _ ← char '}'
          return key
        maybe
          (fail $ printf "key %s was not found in the table" key)
          return
          (Map.lookup key table)
         >>=
         tell ∘ fromText
        takeTillNextSub
        parseAnotherSub
      )
      <|>
      (lift endOfInput)
