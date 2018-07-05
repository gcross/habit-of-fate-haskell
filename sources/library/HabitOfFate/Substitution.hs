{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2017 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should hae received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Substitution where

import HabitOfFate.Prelude

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow(throwM))
import Data.Aeson hiding (Object, (.=))
import Data.Char
import Data.Typeable (Typeable)
import Data.Void
import Language.Haskell.TH.Lift (Lift)
import Text.Parsec hiding ((<|>), optional, uncons)
import Text.XML (Element(..), Name)

import HabitOfFate.TH

uppercase_ ∷ Lens' Char Bool
uppercase_ = lens isUpper (\c → bool (toLower c) (toUpper c))

first_uppercase_ ∷ Traversal' Text Bool
first_uppercase_ = _head . uppercase_

isVowel ∷ Char → Bool
isVowel = (∈ "aeiouAEIOU")

data Kind =
    Name
  | Referrent Referrent
  deriving (Eq, Lift, Ord, Read, Show)

data Referrent =
    Subject
  | Object
  | Possessive
  | ProperPossessive
  | Category
  | CategoryPlural
  | Offspring
  | OffspringPlural
  deriving (Bounded, Enum, Eq, Lift, Ord, Read, Show)

referrents ∷ [(String, Referrent)]
referrents =
  [ ( "he/she", Subject)
  , ( "him/her", Object)
  , ( "his/her", Possessive)
  , ( "his/hers", ProperPossessive)
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

data Atom = Literal Char | Substitution SubstitutionData
  deriving (Eq, Ord, Read, Show)

type Parser = Parsec String ()

getCase ∷ Char → Case
getCase c
  | isUpper c = Upper
  | otherwise = Lower

parseSubstitutionAtom ∷ HasArticle → Maybe Case → Parser Atom
parseSubstitutionAtom article maybe_case = do
  kind ←
    try (string "[" >> pure Name)
    <|>
    msum
      [ try (string word <|> string (word & _head . uppercase_ .~ True) >> char '[')
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
  _ ← char ']'
  pure $ Substitution $
    SubstitutionData
      (article == HasArticle)
      (case_ == Upper)
      kind
      key

parseAtom ∷ Parser Atom
parseAtom = do
  maybe_case_ ← (lookAhead letter <&> (getCase >>> Just)) <|> pure Nothing
  (try $ do
    _ ← char 'a'
    _ ← optional $ char 'n'
    _ ← many1 <<< choice $ map char " \t\r\n"
    parseSubstitutionAtom HasArticle maybe_case_
   )
    <|> (parseSubstitutionAtom HasNoArticle maybe_case_)
    <|> (anyToken <&> Literal)

deriving instance Typeable ParseError
instance Exception ParseError

convertSubstitutionDataToTag ∷ SubstitutionData → Text
convertSubstitutionDataToTag s =
  pack [i|<sub has_article="#{s ^. has_article_}" case="#{s ^. is_uppercase_}" kind="#{s ^. kind_}" key="#{s ^. key_}"/>|]

convertSubstitutionsToTags ∷ MonadThrow m ⇒ String → m Text
convertSubstitutionsToTags story =
  (
    (runParser (many parseAtom ∷ Parser [Atom]) () "<story>" story
    |> either throwM pure)
  )
  <&>
  (
    foldMap (
      \case
        Literal c → singleton c
        Substitution s → convertSubstitutionDataToTag s
    )
  )

data SubstitutionTagParseError =
    SubstitutionTagMustHaveTagSub
  | SubstitutionTagMayNotHaveChildren
  | SubstitutionTagIsMissingAttribute Name
  | SubstitutionTagHasInvalidValueFor Name String
  | SubstitutionTagNotAllowed
  deriving (Eq, Show, Typeable)
instance Exception SubstitutionTagParseError where

parseSubstitutionTag ∷ MonadThrow m ⇒ Element → m SubstitutionData
parseSubstitutionTag (Element tag attrs childs)
  | tag /= "sub" = throwM SubstitutionTagMustHaveTagSub
  | (not <<< null) childs = throwM SubstitutionTagMayNotHaveChildren
  | otherwise =
      SubstitutionData
        <$> attribute "has_article" (unpack >>> readEither)
        <*> attribute "case" (unpack >>> readEither)
        <*> attribute "kind" (unpack >>> readEither)
        <*> attribute "key" Right
  where
    attribute name reader =
      maybe
        (throwM $ SubstitutionTagIsMissingAttribute name)
        (
          reader
          >>>
          either
            (
              SubstitutionTagHasInvalidValueFor name
              >>>
              throwM
            )
            pure
        )
        (lookup name attrs)

parseSubstitutionTagNotAllowed ∷ MonadThrow m ⇒ Element → m Void
parseSubstitutionTagNotAllowed _ = throwM SubstitutionTagNotAllowed

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

lookupAndApplySubstitution ∷ MonadThrow m ⇒ HashMap Text Gendered → SubstitutionData → m Text
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
