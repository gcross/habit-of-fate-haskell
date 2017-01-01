{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.JSON where

import Control.Lens
import Control.Monad ((>=>), when)
import Data.Aeson
  ( FromJSON
  , ToJSON
  , Value(Array, Object, String)
  , parseJSON
  , toJSON
  , withArray
  )
import Data.Aeson.Types (Object, Parser)
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as S
import Data.UUID (UUID, fromText, nil, toText)
import Data.Vector (fromList)
import Text.Printf (printf)

import HabitOfFate.JSONInstances ()
import HabitOfFate.TH
import HabitOfFate.Unicode

_Object ∷ Prism' Value Object
_Object = prism' Object (\case { Object fields → Just fields; _ → Nothing })

data Doc = Doc
  { _links ∷ Maybe (HashMap S.Text S.Text)
  , _data ∷ Value
  } deriving (Read,Show)
deriveJSON ''Doc

data DataObject = DataObject
  { _type ∷ S.Text
  , _id ∷ Maybe UUID
  , _attributes ∷ Value
  } deriving (Eq, Show)
deriveJSON ''DataObject

data ParsedObject α = ParsedObject
  { _maybe_uuid ∷ Maybe UUID
  , _parsed_object ∷ α
  }
makeLenses ''ParsedObject

parseDataObject ∷ FromJSON α ⇒ S.Text → Value → Parser (Maybe UUID, α)
parseDataObject expected_type value = do
  dobj ← parseJSON value
  when (_type dobj /= expected_type) ∘ fail $
    printf "Expected type %s but got type %s" expected_type (_type dobj)
  (_id dobj,) <$> parseJSON (_attributes dobj)

parseDocWithObject ∷ FromJSON α ⇒ S.Text → Value → Parser (Maybe UUID, α)
parseDocWithObject expected_type =
  fmap _data ∘ parseJSON
  >=>
  parseDataObject expected_type

parseDocWithObjects ∷ FromJSON α ⇒ S.Text → Value → Parser (Map UUID α)
parseDocWithObjects expected_type =
  fmap _data ∘ parseJSON
  >=>
  withArray
    "parseDocWithObjects: data value must be an array"
    (fmap Map.fromList
     ∘
     mapM (
        parseDataObject expected_type
        >=>
        \(maybe_uuid, x) →
          case maybe_uuid of
            Nothing → fail "expected id"
            Just uuid → return (uuid, x)
     )
     ∘
     toList)

generateDataObject ∷ ToJSON α ⇒ S.Text → UUID → α → Value
generateDataObject typ uuid x = toJSON $ DataObject typ (Just uuid) (toJSON x)

generateDocWithObject ∷ ToJSON α ⇒ S.Text → S.Text → UUID → α → Value
generateDocWithObject typ self uuid x = toJSON $
  Doc (Just $ HashMap.singleton "self" self) (generateDataObject typ uuid x)

generateDocWithObjects ∷ ToJSON α ⇒ S.Text → S.Text → Map UUID α → Value
generateDocWithObjects typ self =
  toJSON
  ∘
  Doc (Just $ HashMap.singleton "self" self)
  ∘
  Array
  ∘
  fromList
  ∘
  map (uncurry $ generateDataObject typ)
  ∘
  Map.toList
