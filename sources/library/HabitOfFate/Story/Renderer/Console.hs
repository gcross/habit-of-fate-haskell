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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Story.Renderer.Console (renderStoryToChunks) where

import HabitOfFate.Prelude

import Data.Void
import Rainbow

import HabitOfFate.Story

data RenderState = RenderState
  { _render_number_of_columns_ ∷ Int
  , _render_current_word_ ∷ Seq Char
  , _render_saw_spaces_last_ ∷ Bool
  , _render_pending_chunks_ ∷ Seq (Chunk Text)
  , _render_pending_length_ ∷ Int
  }
makeLenses ''RenderState

renderStoryToChunks ∷ Event → [Chunk Text]
renderStoryToChunks [] = []
renderStoryToChunks paragraphs =
  (do mapM_ renderParagraph (intersperse "\n\n" paragraphs)
      tellNewline
  )
  |> execWriter
  |> toList
  where
    tellChunk ∷ MonadWriter (Seq (Chunk Text)) m ⇒ Chunk Text → m ()
    tellChunk = singleton >>> tell

    tellLine ∷ MonadWriter (Seq (Chunk Text)) m ⇒ Text → m ()
    tellLine = (⊢ '\n') >>> chunk >>> tellChunk

    tellNewline ∷ MonadWriter (Seq (Chunk Text)) m ⇒ m ()
    tellNewline = tellLine ""

    renderParagraph ∷ Paragraph → Writer (Seq (Chunk Text)) ()
    renderParagraph =
      go mempty
      >>>
      flip evalStateT (RenderState
        { _render_number_of_columns_ = 0
        , _render_current_word_ = mempty
        , _render_saw_spaces_last_ = False
        , _render_pending_chunks_ = mempty
        , _render_pending_length_ = 0
        })
      where
        go formatting (StyleP style rest) = go (addFormat formatting) rest
          where
            addFormat =
              case style of
                Bold → bold
                Underline → underline
                Color Red → fore red
                Color Blue → fore blue
                Color Green → fore green
                Introduce → bold
        go formatting (MergedP paragraphs) = mapM_ (go formatting) paragraphs
        go formatting (TextP t) =
          (forMOf_ text t $ \c →
            if c ∈ " \t\r\n"
              then do
                saw_spaces_last ← render_saw_spaces_last_ <<.= True
                current_word_is_empty ←
                  (&&)
                    <$> (null <$> use render_current_word_)
                    <*> (null <$> use render_pending_chunks_)
                unless (saw_spaces_last || current_word_is_empty) $ do
                  word ← repack <$> (render_current_word_ <<.= mempty)
                  word_length ← (olength word +) <$> (render_pending_length_ <<.= 0)
                  number_of_columns ← use render_number_of_columns_
                  case number_of_columns of
                    0 → render_number_of_columns_ .= word_length
                    _ | number_of_columns + 1 + word_length >= 80 → do
                          tellNewline
                          render_number_of_columns_ .= word_length
                    _ | otherwise → do
                          tellChunk $ chunk " "
                          render_number_of_columns_ += 1 + word_length
                  (render_pending_chunks_ <<.= mempty) >>= traverse_ tellChunk
                  tellChunk $ formatting ⊕ chunk word
              else do
                render_current_word_ %= (⊢ c)
                render_saw_spaces_last_ .= False
          )
          >>
          (do
            word ← repack <$> (render_current_word_ <<.= mempty)
            render_pending_chunks_ %= (⊢ formatting ⊕ chunk word)
            render_pending_length_ += olength word
          )
        go _ (SubstitutionP sub) = absurd sub
