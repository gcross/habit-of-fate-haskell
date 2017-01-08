{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.TH where

import Control.Lens
import Control.Monad
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

s = QuasiQuoter
  (
    (\case
      [x] → [|pack x|]
      xs → [|map pack xs|]
    )
    ∘
    either
      (error ∘ show)
      toList
    ∘
    runParser parser Seq.empty ""
    ∘
    filter (/= '\r')
  )
  (error "Cannot use q as a pattern")
  (error "Cannot use q as a type")
  (error "Cannot use q as a dec")
  where
    parser = do
      story ← many $ satisfy (/= '=')
      unless (all (∈ " \t\r\n") story) $ modifyState (|> story)
      ((skipMany1 $ char '=') >> parser) <|> getState
