{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.UUID
import Text.Printf

import HabitOfFate.Unicode

instance ToJSON UUID where
  toJSON = String ∘ toText

instance FromJSON UUID where
  parseJSON =
    withText "expected string"
    $
    maybe (fail "invalid UUID") return ∘ fromText

instance ToJSON α ⇒ ToJSON (Map UUID α) where
  toJSON =
    Object
    ∘
    HashMap.fromList
    ∘
    map ((_1 %~ toText) ∘ (_2 %~ toJSON))
    ∘
    Map.toList

instance FromJSON α ⇒ FromJSON (Map UUID α) where
  parseJSON =
    withObject "expected object"
    $
    fmap Map.fromList
    ∘
    mapM (\(key,value) →
      (,) <$> maybe (fail "invalid UUID") return (fromText key)
          <*> parseJSON value
    )
    ∘
    HashMap.toList

newtype JSONCreator α = JSONCreator
  { unwrapJSONCreator ∷ State (HashMap Text Value) α
  } deriving (Applicative,Functor,Monad)

runJSONCreator ∷ JSONCreator () → Value
runJSONCreator = Object ∘ flip execState HashMap.empty ∘ unwrapJSONCreator

add ∷ ToJSON α ⇒ Text → α → JSONCreator ()
add key = JSONCreator ∘ modify ∘ HashMap.insert key ∘ toJSON

addText ∷ Text → Text → JSONCreator ()
addText = add

addObject ∷ Text → JSONCreator () → JSONCreator ()
addObject key = add key ∘ runJSONCreator

type InnerJSONParser α = ReaderT (String, HashMap Text Value) (Except String) α

newtype JSONParser α = JSONParser
  { unwrapJSONParser ∷ InnerJSONParser α
  } deriving (Applicative,Functor,Monad,MonadReader (String, HashMap Text Value))

raiseWith ∷ MonadError String m ⇒ String → String → m α
raiseWith path message = throwError $ printf "Error at path \"%s\": %s" path message

raise ∷ String → InnerJSONParser α
raise message = view _1 >>= flip raiseWith message

instance MonadFail JSONParser where
  fail = JSONParser ∘ raise

runJSONParserWithPath ∷ String → Value → JSONParser α → Either String α
runJSONParserWithPath path value (JSONParser action) = runExcept $ do
  case value of
    Object fields → runReaderT action (path,fields)
    _ → raiseWith path "Expected an object."

runJSONParser ∷ Value → JSONParser α → Either String α
runJSONParser value action = runJSONParserWithPath "" value action

retrieveMaybe ∷ FromJSON α ⇒ Text → JSONParser (Maybe α)
retrieveMaybe key = JSONParser $ do
  s ← view _2
  case HashMap.lookup key s of
    Nothing → return Nothing
    Just value →
      either (raise ∘ printf "Error parsing value \"%s\": %s" (show value)) return
      $
      parseEither parseJSON value

retrieve ∷ FromJSON α ⇒ Text → JSONParser α
retrieve key =
  retrieveMaybe key
  >>=
 maybe (fail $ printf "Unable to find field \"%s\"" key) return

checkTypeIs ∷ Text → JSONParser ()
checkTypeIs expected_type = do
  found_type ← retrieve "type"
  unless (found_type == expected_type) $
    fail $ printf "Expected type \"%s\" but found type \"%s\"" expected_type found_type

checkIdIfPresentIs ∷ UUID → JSONParser ()
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

retrieveObject ∷ Text → JSONParser α → JSONParser α
retrieveObject key action = JSONParser $ do
  path ← view _1
  value ← unwrapJSONParser $ retrieve key
  either throwError return
    $
    runJSONParserWithPath (printf "%s/%s" path key) value action

retrieveObjects ∷ Text → JSONParser α → JSONParser [α]
retrieveObjects key action = JSONParser $ do
  path ← view _1
  values ← unwrapJSONParser $ retrieve key
  forM (zip [0 ∷ Int ..] values) $ \(i, value) →
    either throwError return
    $
    runJSONParserWithPath (printf "%s/%s[%i]" path key i) value action
