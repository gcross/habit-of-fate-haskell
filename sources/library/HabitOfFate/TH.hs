{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.TH where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Aeson.TH (Options(fieldLabelModifier), defaultOptions)
import qualified Data.Aeson.TH as AesonTH
import Data.Foldable
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (pack)
import Language.Haskell.TH.Quote
import Text.Parsec

import HabitOfFate.Unicode

deriveJSON = AesonTH.deriveJSON $ defaultOptions
  { fieldLabelModifier = dropWhile (== '_')
  }

data StoryParserState = StoryParserState
  { _stories ∷ Seq String
  , _current_story ∷ String
  , _current_paragraph ∷ String
  }
makeLenses ''StoryParserState

s = QuasiQuoter
  (
    (\case
      [x] → [|pack x|]
      xs → [|map pack xs|]
    )
    ∘
    either
      (error ∘ show)
      (^. stories . to toList)
    ∘
    flip evalState (StoryParserState Seq.empty "" "")
    ∘
    runParserT (parser >> get) () ""
  )
  (error "Cannot use q as a pattern")
  (error "Cannot use q as a type")
  (error "Cannot use q as a dec")
  where
    parser =
      many
      $
      skipMany (oneOf " \t")
      >>
      -- Assume we are at the start of the line
      choice
        -- Story separator
        [do char '='
            skipMany ∘ satisfy $ (== '\n')
            spaces
            endParagraph
            endStory
        -- Paragraph separator
        ,do endOfLine
            spaces
            endParagraph
        -- Line with text
        ,do line ← many $ satisfy (/= '\n')
            endOfLine
            current_paragraph ⊕= line ⊕ "\n"
        ]

    endParagraph = do
      paragraph ← current_paragraph <<.= ""
      unless (all (∈ " \t\n") paragraph) $
        current_story ⊕= "<p>\n" ⊕ paragraph ⊕ "</p>"

    endStory = do
      story ← current_story <<.= ""
      unless (all (∈ " \t\n") story) $
        stories %= (|> story)
