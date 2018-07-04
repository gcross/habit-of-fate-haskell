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
  deriving (Bounded, Enum, Eq, Lift, Ord, Read, Show)

referrents ∷ [(String, Referrent)]
referrents =
  [ ( "he/she", Subject)
  , ( "him/her", Object)
  , ( "his/her", Possessive)
  , ( "his/hers", ProperPossessive)
  , ( "man/woman", Category)
  , ( "men/women", CategoryPlural)
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

parseSubstitutionAtom ∷ HasArticle → Case → Parser Atom
parseSubstitutionAtom article case_ = do
  kind ←
    try (string "[" >> pure Name)
    <|>
    msum
      [ try (string word <|> string (word & _head . uppercase_ .~ True) >> char '[')
        >>
        (pure $ Referrent referrent)
      | (word, referrent) ← referrents
      ]
  key ← many letter <&> pack
  _ ← char ']'
  pure $ Substitution $
    SubstitutionData
      (article == HasArticle)
      (case_ == Upper)
      kind
      key

getCase ∷ Char → Case
getCase c
  | isUpper c = Upper
  | otherwise = Lower

parseAtom ∷ Parser Atom
parseAtom = do
  case_ ← (lookAhead letter <&> getCase) <|> pure Lower
  (try $ do
    _ ← char 'a'
    _ ← optional $ char 'n'
    _ ← many1 <<< choice $ map char " \t\r\n"
    parseSubstitutionAtom HasArticle case_
   )
    <|> (parseSubstitutionAtom HasNoArticle case_)
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
