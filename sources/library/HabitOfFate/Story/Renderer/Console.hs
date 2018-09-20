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

module HabitOfFate.Story.Renderer.Console (printEvent) where

import HabitOfFate.Prelude

import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import Data.Void
import Rainbow

import HabitOfFate.Story

data RenderState = RenderState
  { _number_of_columns_ ∷ Int
  , _current_word_ ∷ Seq Char
  , _saw_spaces_last_ ∷ Bool
  , _pending_chunks_ ∷ Seq (Chunk Text)
  , _pending_length_ ∷ Int
  }
makeLenses ''RenderState

renderStoryToChunks ∷ Event → [Chunk Text]
renderStoryToChunks [] = []
renderStoryToChunks (first:rest) =
  mconcat >>> toList
  $
  [ renderParagraph first
  , Seq.singleton (chunk "\n")
  , foldMap (renderParagraph >>> (Seq.singleton (chunk "\n\n") ⊕)) rest
  ]
  where
    tellChunk ∷ MonadWriter (Seq (Chunk Text)) m ⇒ Chunk Text → m ()
    tellChunk = singleton >>> tell

    tellLine ∷ MonadWriter (Seq (Chunk Text)) m ⇒ Text → m ()
    tellLine = (⊢ '\n') >>> chunk >>> tellChunk

    tellNewline ∷ MonadWriter (Seq (Chunk Text)) m ⇒ m ()
    tellNewline = tellLine ""

    renderParagraph ∷ Paragraph → Seq (Chunk Text)
    renderParagraph =
      go mempty
      >>>
      flip evalStateT (RenderState
        { _number_of_columns_ = 0
        , _current_word_ = mempty
        , _saw_spaces_last_ = False
        , _pending_chunks_ = mempty
        , _pending_length_ = 0
        })
      >>>
      execWriter
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
                saw_spaces_last ← saw_spaces_last_ <<.= True
                current_word_is_empty ←
                  (&&)
                    <$> (null <$> use current_word_)
                    <*> (null <$> use pending_chunks_)
                unless (saw_spaces_last || current_word_is_empty) $ do
                  word ← repack <$> (current_word_ <<.= mempty)
                  word_length ← (olength word +) <$> (pending_length_ <<.= 0)
                  number_of_columns ← use number_of_columns_
                  case number_of_columns of
                    0 → number_of_columns_ .= word_length
                    _ | number_of_columns + 1 + word_length >= 80 → do
                          tellNewline
                          number_of_columns_ .= word_length
                    _ | otherwise → do
                          tellChunk $ chunk " "
                          number_of_columns_ += 1 + word_length
                  (pending_chunks_ <<.= mempty) >>= traverse_ tellChunk
                  tellChunk $ formatting ⊕ chunk word
              else do
                current_word_ %= (⊢ c)
                saw_spaces_last_ .= False
          )
          >>
          (do
            word ← repack <$> (current_word_ <<.= mempty)
            pending_chunks_ %= (⊢ formatting ⊕ chunk word)
            pending_length_ += olength word
          )
        go _ (SubstitutionP sub) = absurd sub

printEvent ∷ Event → IO ()
printEvent event = do
  printer ← liftIO byteStringMakerFromEnvironment
  event
    |> renderStoryToChunks
    |> chunksToByteStrings printer
    |> traverse_ BS.putStr
    |> liftIO
