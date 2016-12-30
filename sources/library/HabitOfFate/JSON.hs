{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.JSON where

import Control.Lens (Prism', (^?), prism')
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
import qualified Data.Text as S
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.Aeson ()
import Data.Vector (fromList)
import Text.Printf (printf)

import HabitOfFate.TH (deriveJSON)
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
  , _attributes ∷ HashMap S.Text Value
  } deriving (Eq, Show)
deriveJSON ''DataObject

parseDataObject ∷ FromJSON α ⇒ S.Text → Value → Parser α
parseDataObject expected_type value = do
  dobj ← parseJSON value
  when (_type dobj /= expected_type) ∘ fail $
    printf "Expected type %s but got type %s" expected_type (_type dobj)
  parseJSON
    ∘
    Object
    ∘
    (
      HashMap.insert "uuid"
      <$> String ∘ UUID.toText ∘ fromMaybe UUID.nil ∘ _id
      <*> _attributes
    )
    $
    dobj

parseDocWithObject ∷ FromJSON α ⇒ S.Text → Value → Parser α
parseDocWithObject expected_type =
  fmap _data ∘ parseJSON
  >=>
  parseDataObject expected_type

parseDocWithObjects ∷ FromJSON α ⇒ S.Text → Value → Parser [α]
parseDocWithObjects expected_type =
  fmap _data ∘ parseJSON
  >=>
  withArray
    "parseDocWithObjects: data value must be an array"
    (mapM (parseDataObject expected_type) ∘ toList)

generateDataObject ∷ ToJSON α ⇒ S.Text → α → Value
generateDataObject typ x =
  toJSON
  $
  DataObject
    typ
    (case HashMap.lookup "uuid" fields of
      Nothing → error "generateDataObject: given object did not have uuid field"
      Just (String uuid) → UUID.fromText uuid
      Just v → error $ "generateDataObject: the uuid field must be a string, not " ⊕ show v
    )
    (HashMap.delete "uuid" fields)
  where
    fields =
      fromMaybe
        (error "generateDataObject applied to a non-object value")
        (toJSON x ^? _Object)

generateDocWithObject ∷ ToJSON α ⇒ S.Text → S.Text → α → Value
generateDocWithObject typ self x = toJSON $
  Doc (Just $ HashMap.singleton "self" self) (generateDataObject typ x)

generateDocWithObjects ∷ (Foldable t, ToJSON α) ⇒ S.Text → S.Text → t α → Value
generateDocWithObjects typ self =
  toJSON
  ∘
  Doc (Just $ HashMap.singleton "self" self)
  ∘
  Array
  ∘
  fromList
  ∘
  map (generateDataObject typ)
  ∘
  toList
