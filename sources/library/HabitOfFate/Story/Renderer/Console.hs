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

import Rainbow

import HabitOfFate.Story

data RenderState = RenderState
  { _render_number_of_columns ∷ Int
  , _render_current_word ∷ Seq Char
  , _render_saw_spaces_last ∷ Bool
  , _render_pending_chunks ∷ Seq (Chunk Text)
  , _render_pending_length ∷ Int
  }
makeLenses ''RenderState

renderStoryToChunks ∷ Story → [Chunk Text]
renderStoryToChunks =
  dropEmptyThingsFromStory
  >>>
  unwrapGenStory
  >>>
  uncons
  >>>
  maybe
    (return ())
    (
      \(first, rest) → do
        tellEventSeparator
        renderQuest first
        forM_ rest $ \quest → do
          tellQuestSeparator
          tellLine "A new quest begins..."
          tellQuestSeparator
          renderQuest quest
        tellEventSeparator
    )
  >>>
  execWriter
  >>>
  toList
  where
    tellChunk ∷ MonadWriter (Seq (Chunk Text)) m ⇒ Chunk Text → m ()
    tellChunk = singleton >>> tell

    tellLine ∷ MonadWriter (Seq (Chunk Text)) m ⇒ Text → m ()
    tellLine = (⊢ '\n') >>> chunk >>> tellChunk

    tellNewline ∷ MonadWriter (Seq (Chunk Text)) m ⇒ m ()
    tellNewline = tellLine ""

    tellSeparator ∷ MonadWriter (Seq (Chunk Text)) m ⇒ Char → m ()
    tellSeparator = replicate 80 >>> tellLine

    tellEventSeparator = tellSeparator '―'
    tellQuestSeparator = tellSeparator '═'

    renderQuest = unwrapGenQuest >>> go
      where
        go [] = return ()
        go (x:[]) = renderEvent x
        go (x:xs) = do
          renderEvent x
          tellEventSeparator
          go xs

    renderEvent = unwrapGenEvent >>> go
      where
        go [] = return ()
        go (x:[]) = do
          renderParagraph x
          tellNewline
        go (x:xs) = do
          renderParagraph x
          tellNewline
          tellNewline
          go xs

    renderParagraph ∷ Paragraph → Writer (Seq (Chunk Text)) ()
    renderParagraph =
      go mempty
      >>>
      flip evalStateT (RenderState
        { _render_number_of_columns = 0
        , _render_current_word = mempty
        , _render_saw_spaces_last = False
        , _render_pending_chunks = mempty
        , _render_pending_length = 0
        })
      where
        go formatting (Style style rest) = go (addFormat formatting) rest
          where
            addFormat =
              case style of
                Bold → bold
                Underline → underline
                Color Red → fore red
                Color Blue → fore blue
                Color Green → fore green
                Introduce → bold
        go formatting (Merged paragraphs) = mapM_ (go formatting) paragraphs
        go formatting (Text_ t) =
          (forMOf_ text t $ \c →
            if c ∈ " \t\r\n"
              then do
                saw_spaces_last ← render_saw_spaces_last <<.= True
                current_word_is_empty ←
                  (&&)
                    <$> (null <$> use (render_current_word))
                    <*> (null <$> use (render_pending_chunks))
                unless (saw_spaces_last || current_word_is_empty) $ do
                  word ← repack <$> (render_current_word <<.= mempty)
                  word_length ← (olength word +) <$> (render_pending_length <<.= 0)
                  number_of_columns ← use (render_number_of_columns)
                  case number_of_columns of
                    0 → render_number_of_columns .= word_length
                    _ | number_of_columns + 1 + word_length >= 80 → do
                          tellNewline
                          render_number_of_columns .= word_length
                    _ | otherwise → do
                          tellChunk $ chunk " "
                          render_number_of_columns += 1 + word_length
                  (render_pending_chunks <<.= mempty) >>= traverse_ tellChunk
                  tellChunk $ formatting ⊕ chunk word
              else do
                render_current_word %= (⊢ c)
                render_saw_spaces_last .= False
          )
          >>
          (do
            word ← repack <$> (render_current_word <<.= mempty)
            render_pending_chunks %= (⊢ formatting ⊕ chunk word)
            render_pending_length += olength word
          )
