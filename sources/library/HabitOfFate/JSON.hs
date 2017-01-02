{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.JSON where

import Prelude hiding (fail)

import Control.Lens
import Control.Monad.Except hiding (fail)
import Control.Monad.Fail
import Control.Monad.Reader hiding (fail)
import Control.Monad.State hiding (fail)
import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as S
import Data.UUID (UUID)
import Text.Printf

import HabitOfFate.JSONInstances ()
import HabitOfFate.Unicode

newtype MakeJSON α = MakeJSON
  { unwrapMakeJSON ∷ State (HashMap S.Text Value) α
  } deriving (Applicative,Functor,Monad)

makeJSON ∷ MakeJSON () → Value
makeJSON = Object ∘ flip execState HashMap.empty ∘ unwrapMakeJSON

add ∷ ToJSON α ⇒ S.Text → α → MakeJSON ()
add key = MakeJSON ∘ modify ∘ HashMap.insert key ∘ toJSON

addText ∷ S.Text → S.Text → MakeJSON ()
addText = add

addObject ∷ S.Text → MakeJSON () → MakeJSON ()
addObject key = add key ∘ makeJSON

type InnerUnmakeJSON α = ReaderT (String, HashMap S.Text Value) (Except String) α

newtype UnmakeJSON α = UnmakeJSON
  { unwrapUnmakeJSON ∷ InnerUnmakeJSON α
  } deriving (Applicative,Functor,Monad,MonadReader (String, HashMap S.Text Value))

raiseWith ∷ MonadError String m ⇒ String → String → m α
raiseWith path message = throwError $ printf "Error at path \"%s\": %s" path message

raise ∷ String → InnerUnmakeJSON α
raise message = view _1 >>= flip raiseWith message

instance MonadFail UnmakeJSON where
  fail = UnmakeJSON ∘ raise

unmakeJSONWithPath ∷ String → Value → UnmakeJSON α → Either String α
unmakeJSONWithPath path value (UnmakeJSON action) = runExcept $ do
  case value of
    Object fields → runReaderT action (path,fields)
    _ → raiseWith path "Expected an object."

unmakeJSON ∷ Value → UnmakeJSON α → Either String α
unmakeJSON value action = unmakeJSONWithPath "" value action

retrieveMaybe ∷ FromJSON α ⇒ S.Text → UnmakeJSON (Maybe α)
retrieveMaybe key = UnmakeJSON $ do
  s ← view _2
  case HashMap.lookup key s of
    Nothing → return Nothing
    Just value →
      either (raise ∘ printf "Error parsing value \"%s\": %s" (show value)) return
      $
      parseEither parseJSON value

retrieve ∷ FromJSON α ⇒ S.Text → UnmakeJSON α
retrieve key =
  retrieveMaybe key
  >>=
 maybe (fail $ printf "Unable to find field \"%s\"" key) return

checkTypeIs ∷ S.Text → UnmakeJSON ()
checkTypeIs expected_type = do
  found_type ← retrieve "type"
  unless (found_type == expected_type) $
    fail $ printf "Expected type \"%s\" but found type \"%s\"" expected_type found_type

checkIdIfPresentIs ∷ UUID → UnmakeJSON ()
checkIdIfPresentIs expected_uuid =
  retrieveMaybe "id" >>= \case
    Just uuid | uuid /= expected_uuid →
      fail
      $
      printf
        "the id of the object in the doc (%s) does not match the expected id (%s)"
        (show uuid)
        (show expected_uuid)
    _ → return ()

retrieveObject ∷ S.Text → UnmakeJSON α → UnmakeJSON α
retrieveObject key action = UnmakeJSON $ do
  path ← view _1
  value ← unwrapUnmakeJSON $ retrieve key
  either throwError return
    $
    unmakeJSONWithPath (printf "%s/%s" path key) value action

retrieveObjects ∷ S.Text → UnmakeJSON α → UnmakeJSON [α]
retrieveObjects key action = UnmakeJSON $ do
  path ← view _1
  values ← unwrapUnmakeJSON $ retrieve key
  forM (zip [0 ∷ Int ..] values) $ \(i, value) →
    either throwError return
    $
    unmakeJSONWithPath (printf "%s/%s[%i]" path key i) value action
