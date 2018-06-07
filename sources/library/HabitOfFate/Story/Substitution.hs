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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Story.Substitution where

import HabitOfFate.Prelude

import Data.Aeson hiding ((.=))
import qualified Data.Char as Char
import Text.Parsec hiding ((<|>), optional, uncons)

import HabitOfFate.Story
import HabitOfFate.TH

data Gender = Male | Female deriving (Enum,Eq,Ord,Read,Show)

instance ToJSON Gender where
  toJSON gender = String $
    case gender of
      Male → "male"
      Female → "female"

instance FromJSON Gender where
  parseJSON = withText "expected text" parseGender
    where
      parseGender "male" = return Male
      parseGender "female" = return Female
      parseGender wrong = fail [i|gender must be "male" or "female", not "#{wrong}"|]

data Gendered = Gendered
  { _gendered_name ∷ Text
  , _gendered_gender ∷ Gender
  } deriving (Eq,Ord,Read,Show)
deriveJSONDropping 10 ''Gendered
makeLenses ''Gendered

_case ∷ Lens' Char Bool
_case = lens Char.isUpper (\c → bool (Char.toLower c) (Char.toUpper c))

first_case ∷ Traversal' Text Bool
first_case = _head . _case

findNounConverter ∷ Text → Maybe (Gender → Text)
findNounConverter "" = Nothing
findNounConverter word
  | word ∈ ["His", "Her"] = Just $ \case { Male → "His"; Female → "Her" }
  | otherwise =
      find (elemOf both (word & first_case .~ False)) nouns
      <&>
      (\(m,f) →
        \case { Male → m; Female → f }
        >>>
        first_case .~ (word ^?! first_case)
        >>>
        takeWhile (/= '|')
      )
  where
    nouns =
      [ ("he","she")
      , ("him","her|obj")
      , ("his|pp","hers")
      , ("himself","herself")
      , ("his","her|pos")
      , ("man","woman")
      , ("son","daughter")
      ]

type Substitutor = Text → Either String Paragraph

substitute ∷ Substitutor → SubParagraph → Either String Paragraph
substitute substitutor = replaceTextM substituteIn
  where
    substituteIn (Literal t) = return $ Text_ t
    substituteIn (Key key) = substitutor key

isVowel ∷ Char → Bool
isVowel = (∈ "aeiouAEIOU")

makeSubstitutor ∷ HashMap Text Gendered → HashMap Text Text → Substitutor
makeSubstitutor _ _ "" = error "empty keys are not supported"
makeSubstitutor gendered neutered key =
  key
    |> runParser parser () ""
    |> bimap show Text_
  where
    parser =
      (do starts_with_uppercase ← try $ do
            starts_with_uppercase ← Char.isUpper <$> oneOf "Aa"
            _ ← optional (char 'n')
            _ ← space
            return starts_with_uppercase
          neutered_name ← (rewords >>> pack) <$> many1 letter
          name ←
            maybe
              (fail [i|Unable to find neuter entity with name "#{neutered_name}".|])
              return
            $
            lookup neutered_name neutered
          return $ mconcat
            [ if starts_with_uppercase then "A" else "a"
            , if fromMaybe False (isVowel <$> name ^? _head) then "n " else " "
            , name
            ]
      )
      <|>
      (do word ← pack <$> many1 (letter <|> char '|')
          maybe_name ←
            optionMaybe
            $
            between
              (char '[')
              (char ']')
              ((rewords >>> pack) <$> many (letter <|> space))
          case findNounConverter word of
            Nothing →
              maybe (fail [i|unrecognized word "#{word}"|]) return
              $
              (view gendered_name <$> lookup word gendered) <|> lookup word neutered
            Just convertNoun → do
              let tryName name message =
                    lookup name gendered
                    |>
                    maybe
                      (fail message)
                      (
                        view gendered_gender
                        >>>
                        convertNoun
                        >>>
                        return
                      )
              case maybe_name of
                Nothing →
                  tryName "" "no default entity provided"
                Just name →
                  tryName name [i|unable to find entity with name "#{name}"|]
      )
