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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import HabitOfFate.Prelude hiding (elements)

import Control.Lens.Extras
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Test.Tasty.SmallCheck as S
import Test.SmallCheck.Series

import HabitOfFate.Story
import HabitOfFate.Story.Parser.XML
import HabitOfFate.Story.Renderer.XML

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary Color where
  arbitrary = elements [Red, Green, Blue]

instance Monad m ⇒ Serial m Color where

instance Arbitrary Style where
  arbitrary = oneof
    [ return Bold
    , return Underline
    , Color <$> arbitrary
    , return Introduce
    ]

instance Monad m ⇒ Serial m Style where

instance Arbitrary α ⇒ Arbitrary (GenParagraph α) where
  arbitrary = sized $ \n →
    if n <= 1
      then Text_ <$> arbitrary
      else oneof
        [ Style <$> arbitrary <*> resize (n-1) arbitrary
        , Merged <$> (do
            nc ← choose (1,n)
            fromList <$> vectorOf nc (resize (n `div` nc) arbitrary)
          )
        , Text_ <$> arbitrary
        ]
  shrink (Style _ child) = [child]
  shrink (Merged children) = toList children
  shrink (Text_ _) = []

instance (Monad m, Serial m α) ⇒ Serial m (Seq α) where
  series = fromList <$> series

instance Monad m ⇒ Serial m Text where
  series = pack <$> series

instance Monad m ⇒ Serial m SubText where

instance (Monad m, Serial m α) ⇒ Serial m (GenParagraph α) where

originalFromSubParagraph ∷ SubParagraph → Text
originalFromSubParagraph =
  foldMap (
    \case
      Literal t → t
      Key k → "{" ⊕ k ⊕ "}"
  )
  >>>
  rewords

originalFromSubEvent ∷ SubEvent → Text
originalFromSubEvent =
  map originalFromSubParagraph
  >>>
  intersperse "\n"
  >>>
  mconcat

storyToLists = toList >>> map questToLists
questToLists = toList

main = defaultMain $ testGroup "All Tests"
  [ testGroup "HabitOfFate.Story"
    [ testGroup "round-trip"
      [ testGroup "Paragraph -> [Node] -> Paragraph" $
          let doTest story =
                ( is _Right double_round_trip_xml_text
                  &&
                  (double_round_trip_xml_text & _Left %~ show) ==
                    (round_trip_xml_text & _Left %~ show)
                , message
                )
                where
                  xml_text = renderStoryToXMLText story
                  round_trip_story = parseStoryFromText xml_text
                  round_trip_xml_text = renderStoryToXMLText <$> round_trip_story
                  double_round_trip_story = round_trip_xml_text >>= parseStoryFromText
                  double_round_trip_xml_text = renderStoryToXMLText <$> double_round_trip_story
                  message = unlines
                    ["ORIGINAL STORY:"
                    ,"    Right " ⊕ show (storyToLists story)
                    ,"ROUND-TRIP STORY:"
                    ,"    " ⊕ show (fmap storyToLists round_trip_story)
                    ,"DOUBLE ROUND-TRIP STORY:"
                    ,"    " ⊕ show (fmap storyToLists double_round_trip_story)
                    ,"ORIGINAL XML:"
                    ,"    Right " ⊕ show xml_text
                    ,"ROUND-TRIP XML:"
                    ,"    " ⊕ show round_trip_xml_text
                    ,"DOUBLE ROUND-TRIP XML:"
                    ,"    " ⊕ show double_round_trip_xml_text
                    ]
          in
          [ S.testProperty "SmallCheck" $ \story →
              let (result, message) = doTest story
              in if result then Right ("" ∷ String) else Left message
          , localOption (QuickCheckMaxSize 20)
            $
            testProperty "QuickCheck" $ \story →
              let (result, message) = doTest story
              in counterexample message result
          ]
      ]
    ]
  ]
