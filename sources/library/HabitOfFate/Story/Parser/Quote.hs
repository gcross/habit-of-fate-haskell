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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Story.Parser.Quote (s, s_fixed) where

import HabitOfFate.Prelude

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow(throwM))
import qualified Data.Text.Lazy as Lazy
import Data.Typeable (Typeable)
import qualified Language.Haskell.TH.Lift as Lift
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Text.Parsec hiding ((<|>), optional, uncons)

import HabitOfFate.Story
import HabitOfFate.Story.Parser.XML

insertMarkers ∷ String → String
insertMarkers =
  lines
  >>>
  fmap (
    dropWhile (∈ " \t")
    >>>
    \case
      "" → "</p><p>"
      '=':_ → "</p></event><event><p>"
      line → line
  )
  >>>
  (\x → ["<story><quest><event><p>"] ⊕ x ⊕ ["</p></event></quest></story>"])
  >>>
  unlines

data ParseSubstitutionException = ParseSubstitutionException String deriving (Eq,Show,Typeable)
instance Exception ParseSubstitutionException where

parseSubstitutions ∷ ∀ m. MonadThrow m ⇒ Paragraph → m SubParagraph
parseSubstitutions =
  replaceTextM parseSubstitutionsIn >>> runWriterT >>> fmap fst
  where
    parseSubstitutionsIn ∷ Text → WriterT (Set Text) m SubParagraph
    parseSubstitutionsIn chunk =
      runParserT parser () "" chunk
      >>=
      either
        (\msg → throwM $
          ParseSubstitutionException [i|Error parsing substitutions for text chunk "%{chunk}": #{msg}|])
        return

    parser =
      mappend
        <$> takeTillNextSub
        <*> (mconcat <$> (many $ mappend <$> parseAnotherSub <*> takeTillNextSub))

    takeTillNextSub = (pack >>> Literal >>> Text_) <$> many (satisfy (/='{'))

    parseAnotherSub = do
      key ←
        pack
        <$>
        between
          (char '{')
          (char '}')
          ((rewords >>> pack) <$> (many1 $ letter <|> char '|' <|> space))
      tell >>> lift $ singletonSet key
      key |> Key |> Text_ |> return

parseQuote ∷ String → [SubEvent]
parseQuote =
  insertMarkers
  >>>
  Lazy.pack
  >>>
  (
    parseStoryFromText
    >=>
    (mapM $ traverseOf (events . paragraphs) parseSubstitutions)
    >=>
    \case
      [quest] → return $ unwrapGenQuest quest
      xs → throwM $ ParseSubstitutionException [i|saw #{olength xs} quests instead of 1|]
  )
  >>>
  either (show >>> error) identity

s ∷ QuasiQuoter
s = QuasiQuoter
  (parseQuote >>> Lift.lift)
  (error "Cannot use s as a pattern")
  (error "Cannot use s as a type")
  (error "Cannot use s as a dec")

s_fixed ∷ QuasiQuoter
s_fixed = QuasiQuoter
  (
    parseQuote
    >>>
    (\case
      [] → [|()|]
      [x1] → [|x1|]
      [x1,x2] → [|(x1,x2)|]
      [x1,x2,x3] → [|(x1,x2,x3)|]
      [x1,x2,x3,x4] → [|(x1,x2,x3,x4)|]
      [x1,x2,x3,x4,x5] → [|(x1,x2,x3,x4,x5)|]
      [x1,x2,x3,x4,x5,x6] → [|(x1,x2,x3,x4,x5,x6)|]
      [x1,x2,x3,x4,x5,x6,x7] → [|(x1,x2,x3,x4,x5,x6,x7)|]
      [x1,x2,x3,x4,x5,x6,x7,x8] → [|(x1,x2,x3,x4,x5,x6,x7,x8)|]
      [x1,x2,x3,x4,x5,x6,x7,x8,x9] → [|(x1,x2,x3,x4,x5,x6,x7,x8,x9)|]
      [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10] → [|(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)|]
      xs → error [i|saw #{olength xs} events, which is too many (> 10)|]
    )
  )
  (error "Cannot use s1 as a pattern")
  (error "Cannot use s1 as a type")
  (error "Cannot use s1 as a dec")
