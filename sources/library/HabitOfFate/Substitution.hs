{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2017 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Substitution
  (
  -- Exceptions
    SubstitutionException(..)
  , ParseError

  -- Regular types + lenses
  , Gender(..)
  , Gendered(..)
      , gendered_name_
      , gendered_gender_
  , Story
  , Substitutions

  -- Functions
  , extractPlaceholders
  , parseSubstitutions
  , substitute
  ) where

import HabitOfFate.Prelude

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow(throwM))
import Data.Aeson hiding (Object, (.=))
import Data.Char
import Data.MonoTraversable (Element)
import Data.String (IsString(..))
import Language.Haskell.TH.Lift (Lift)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import Data.Typeable (Typeable)
import Text.Parsec hiding ((<|>), optional, uncons)

import HabitOfFate.TH

uppercase_ ∷ Lens' Char Bool
uppercase_ = lens isUpper (\c → bool (toLower c) (toUpper c))
first_uppercase_ ∷ Traversal' Text Bool
first_uppercase_ = _head . uppercase_

isVowel ∷ Char → Bool
isVowel = (∈ "aeiouAEIOU")

data Referrent =
    Subject
  | Object
  | Possessive
  | ProperPossessive
  | Reflexive
  | Category
  | CategoryPlural
  | Offspring
  | OffspringPlural
  deriving (Bounded, Enum, Eq, Lift, Ord, Read, Show)

data Kind =
    Name
  | Referrent Referrent
  deriving (Eq, Lift, Ord, Read, Show)

referrents ∷ [(String, Referrent)]
referrents =
  [ ( "he/she", Subject)
  , ( "him/her", Object)
  , ( "his/her", Possessive)
  , ( "his/hers", ProperPossessive)
  , ( "himself/herself", Reflexive)
  , ( "man/woman", Category)
  , ( "men/women", CategoryPlural)
  , ( "son/daughter", Offspring)
  , ( "sons/daughters", OffspringPlural)
  ]

data HasArticle = HasArticle | HasNoArticle
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Case = Upper | Lower
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data SubstitutionData = SubstitutionData
  { _has_article_ ∷ Bool
  , _is_uppercase_ ∷ Bool
  , _kind_ ∷ Kind
  , _key_ ∷ Text
  }
  deriving (Eq, Lift, Ord, Read, Show)
makeLenses ''SubstitutionData

data Chunk α = Literal α | Substitution SubstitutionData
  deriving (Eq, Functor, Lift, Ord, Read, Show)
type instance Element Story = Chunk Text
newtype Story = Story { unwrapStory ∷ [Chunk Text] }
 deriving
  ( Default
  , Eq
  , Lift
  , MonoFoldable
  , MonoPointed
  , Monoid
  , Ord
  , Read
  , Semigroup
  , Show
  )

instance IsString Story where
  fromString = fromString >>> Literal >>> singleton

extractPlaceholders ∷ Story → HashSet Text
extractPlaceholders =
  unwrapStory
  >>>
  mapMaybe (\case { Literal _ → Nothing; Substitution s → Just (s ^. key_) })
  >>>
  setFromList

type Parser = Parsec String ()

getCase ∷ Char → Case
getCase c
  | isUpper c = Upper
  | otherwise = Lower

parseSubstitutionChunk ∷ HasArticle → Maybe Case → Parser (Chunk Char)
parseSubstitutionChunk article maybe_case = do
  kind ←
    try (string "|" >> pure Name)
    <|>
    msum
      [ try (string word <|> string (word & _head . uppercase_ .~ True) >> char '|')
        >>
        (pure $ Referrent referrent)
      | (word, referrent) ← referrents
      ]
  case_ ←
    maybe
      ((lookAhead letter <&> getCase) <|> pure Upper)
      pure
      maybe_case
  key ← many letter <&> pack
  pure $ Substitution $
    SubstitutionData
      (article == HasArticle)
      (case_ == Upper)
      kind
      key

parseChunk ∷ Parser (Chunk Char)
parseChunk = do
  maybe_case_ ← (lookAhead letter <&> (getCase >>> Just)) <|> pure Nothing
  (try $ do
    _ ← char 'a' <|> char 'A'
    _ ← optional $ char 'n'
    _ ← many1 <<< choice $ map char " \t\r\n"
    parseSubstitutionChunk HasArticle maybe_case_
   )
    <|> parseSubstitutionChunk HasNoArticle maybe_case_
    <|> (anyToken <&> Literal)

instance Exception ParseError

parseSubstitutions ∷ MonadThrow m ⇒ String → m Story
parseSubstitutions story =
  (
    runParser (many parseChunk ∷ Parser [Chunk Char]) () "<story>" story
    |> either throwM pure
  )
  <&>
  (mergeChunks >>> map ((pack >>> Text.reverse) <$>) >>> Story)
 where
  mergeChunks ∷ [Chunk Char] → [Chunk [Char]]
  mergeChunks [] = []
  mergeChunks (first:rest) = go (singleton <$> first) rest
   where
    go ∷ Chunk [Char] → [Chunk Char] → [Chunk [Char]]
    go previous [] = previous:[]
    go previous (next@(Substitution _):rest) = previous:go (singleton <$> next) rest
    go previous@(Substitution _) (next:rest) = previous:go (singleton <$> next) rest
    go (Literal x) (Literal y:rest) = go (Literal (y:x)) rest

data Gender = Male | Female | Neuter deriving (Enum,Eq,Ord,Read,Show)

instance ToJSON Gender where
  toJSON gender = String $
    case gender of
      Male → "male"
      Female → "female"
      Neuter → "neuter"

instance FromJSON Gender where
  parseJSON = withText "expected text" parseGender
    where
      parseGender "male" = return Male
      parseGender "female" = return Female
      parseGender "neuter" = return Neuter
      parseGender wrong = fail [i|gender must be "male", "female", or "neuter", not "#{wrong}"|]

data Gendered = Gendered
  { _gendered_name_ ∷ Text
  , _gendered_gender_ ∷ Gender
  } deriving (Eq,Ord,Read,Show)
deriveJSONDropping 10 ''Gendered
makeLenses ''Gendered

data SubstitutionException =
    NoSuchKeyException Text
  | EmptyNameException
  deriving (Eq, Ord, Read, Show)
instance Exception SubstitutionException

type Substitutions = HashMap Text Gendered

lookupAndApplySubstitution ∷ MonadThrow m ⇒ Substitutions → SubstitutionData → m Text
lookupAndApplySubstitution table s = do
  gendered@(Gendered name _) ←
    maybe
      (throwM $ NoSuchKeyException $ s ^. key_)
      pure
      (lookup (s ^. key_) table)
  when (onull name) $ throwM EmptyNameException
  let article
        | s ^. has_article_ =
            if isVowel (name ^?! _head)
              then "an "
              else "a "
        | otherwise = ""
      word = applyKind (s ^. kind_) gendered
  pure $
    (article ⊕ word) & first_uppercase_ .~ (s ^. is_uppercase_)

data KeyError = KeyError Text deriving (Show, Typeable)
instance Exception KeyError

substitute ∷ MonadThrow m ⇒ Substitutions → Story → m Lazy.Text
substitute table (Story text) =
  mapM
    (\case
      Literal l → pure l
      Substitution s → lookupAndApplySubstitution table s
    )
    text
  <&>
  Lazy.fromChunks

applyKind ∷ Kind → Gendered → Text
applyKind Name (Gendered name _) = name
applyKind (Referrent referrent) (Gendered _ gender) =
  applyReferrent referrent gender

applyReferrent ∷ Referrent → Gender → Text

applyReferrent Subject Male = "he"
applyReferrent Subject Female = "she"
applyReferrent Subject Neuter = "it"

applyReferrent Object Male = "him"
applyReferrent Object Female = "her"
applyReferrent Object Neuter = "it"

applyReferrent Possessive Male = "his"
applyReferrent Possessive Female = "her"
applyReferrent Possessive Neuter = "its"

applyReferrent ProperPossessive Male = "his"
applyReferrent ProperPossessive Female = "hers"
applyReferrent ProperPossessive Neuter = "its"

applyReferrent Reflexive Male = "himself"
applyReferrent Reflexive Female = "herself"
applyReferrent Reflexive Neuter = "itsself"

applyReferrent Category Male = "man"
applyReferrent Category Female = "woman"
applyReferrent Category Neuter = "thing"

applyReferrent CategoryPlural Male = "men"
applyReferrent CategoryPlural Female = "women"
applyReferrent CategoryPlural Neuter = "things"

applyReferrent Offspring Male = "son"
applyReferrent Offspring Female = "daughter"
applyReferrent Offspring Neuter = "offspring"

applyReferrent OffspringPlural Male = "sons"
applyReferrent OffspringPlural Female = "daughters"
applyReferrent OffspringPlural Neuter = "offspring"
