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

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module HabitOfFate.Story where

import HabitOfFate.Prelude

import Data.Aeson hiding ((.=))
import qualified Data.Char as Char
import Data.String (IsString(..))
import qualified Data.Text.Lazy as LazyText
import Instances.TH.Lift ()
import GHC.Generics hiding (from, to)
import Language.Haskell.TH.Lift (Lift)
import qualified Language.Haskell.TH.Lift as Lift
import Language.Haskell.TH.Quote
import Rainbow
import Text.XML
import Text.Parsec hiding ((<|>), optional, uncons)

import HabitOfFate.TH

data Color = Red | Green | Blue deriving (Enum,Eq,Generic,Lift,Ord,Read,Show)

data Style = Bold | Underline | Color Color | Introduce deriving (Eq,Generic,Lift,Ord,Read,Show)

data GenParagraph α =
    Style Style (GenParagraph α)
  | Merged (Seq (GenParagraph α))
  | Text_ α
  deriving (Eq,Foldable,Functor,Generic,Lift,Ord,Read,Show,Traversable)

replaceTextM ∷ Applicative f ⇒ (α → f (GenParagraph β)) → GenParagraph α → f (GenParagraph β)
replaceTextM f (Style s x) = Style s <$> replaceTextM f x
replaceTextM f (Merged xs) = Merged <$> traverse (replaceTextM f) xs
replaceTextM f (Text_ t) = f t

newtype GenEvent α = GenEvent { unwrapGenEvent ∷ [GenParagraph α] }
  deriving (Eq,Generic,Lift,Monoid,Ord,Read,Show)
makeWrapped ''GenEvent

newtype GenQuest α = GenQuest { unwrapGenQuest ∷ [GenEvent α] }
  deriving (Eq,Generic,Lift,Monoid,Ord,Read,Show)
makeWrapped ''GenQuest

newtype GenStory α = GenStory { unwrapGenStory ∷ [GenQuest α] }
  deriving (Eq,Generic,Lift,Monoid,Ord,Read,Show)
makeWrapped ''GenStory

paragraphs ∷ IndexedTraversal Int (GenEvent α) (GenEvent β) (GenParagraph α) (GenParagraph β)
paragraphs f (GenEvent ps) = GenEvent <$> (traversed f ps)

events ∷ IndexedTraversal Int (GenQuest α) (GenQuest β) (GenEvent α) (GenEvent β)
events f (GenQuest ps) = GenQuest <$> (traversed f ps)

quests ∷ IndexedTraversal Int (GenStory α) (GenStory β) (GenQuest α) (GenQuest β)
quests f (GenStory ps) = GenStory <$> (traversed f ps)

createEvent ∷ Foldable t ⇒ t Paragraph → Event
createEvent = toList >>> GenEvent

createQuest ∷ Foldable t ⇒ t Event → Quest
createQuest = toList >>> GenQuest

createStory ∷ Foldable t ⇒ t Quest → Story
createStory = toList >>> GenStory

eventToLists ∷ GenEvent α → [GenParagraph α]
eventToLists = unwrapGenEvent

questToLists ∷ GenQuest α → [[GenParagraph α]]
questToLists = unwrapGenQuest >>> fmap eventToLists

storyToLists ∷ GenStory α → [[[GenParagraph α]]]
storyToLists = unwrapGenStory >>> fmap questToLists

type Paragraph = GenParagraph Text
type Event = GenEvent Text
type Quest = GenQuest Text
type Story = GenStory Text

instance IsString Paragraph where
  fromString = pack >>> Text_

data SubText = Key Text | Literal Text deriving (Eq,Generic,Lift,Ord,Read,Show)
makePrisms ''SubText

type SubParagraph = GenParagraph SubText
type SubEvent = GenEvent SubText
type SubQuest = GenQuest SubText
type SubStory = GenStory SubText

class HasLiterals α where
  literals ∷ IndexedFold Int (GenParagraph α) Text

instance HasLiterals Text where
  literals = folded

instance HasLiterals SubText where
  literals = folded . _Literal

class GenText α where
  textIsNull ∷ α → Bool
  textIsAllSpaces ∷ α → Bool

instance GenText Text where
  textIsNull = onull

  textIsAllSpaces = allSpaces

instance GenText SubText where
  textIsNull (Literal t) = onull t
  textIsNull _ = False

  textIsAllSpaces (Literal t) = allSpaces t
  textIsAllSpaces _ = False

instance GenText α ⇒ Monoid (GenParagraph α) where
  mempty = Merged mempty
  mappend (Text_ x) ys | textIsNull x = ys
  mappend xs (Text_ y) | textIsNull y = xs
  mappend (Merged xs) (Merged ys) = xs ⊕ ys |> Merged
  mappend (Merged xs) y = xs ⊢ y |> Merged
  mappend x (Merged ys) = x ⊣ ys |> Merged
  mappend x y = [x,y] |> fromList |> Merged

textFromParagraph ∷ Paragraph → Text
textFromParagraph = fold

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

allSpaces ∷ Text → Bool
allSpaces = allOf text (∈ " \t\r\n")

parseContainer ∷ Text → ([Node] → Either String α) → Node → Either String α
parseContainer expected_tag parseChildren node =
  case node of
    NodeInstruction _ → fail "unexpected XML instruction"
    NodeComment _ → parseChildren []
    NodeContent t
      | allSpaces t → parseChildren []
      | otherwise → fail $ "unexpected non-whitespace text outside of <p>"
    NodeElement (Element (Name tag _ _) attrs children)
      | tag /= expected_tag →
          fail [i|expected <#{expected_tag}> but got <#{tag}>|]
      | attrs |> (not <<< null) →
          fail [i|expected no attributes in <#{tag}>"|]
      | otherwise → parseChildren children

parseStoryFromNodes ∷ [Node] → Either String Story
parseStoryFromNodes =
  mapM (parseContainer "quest" parseQuestFromNodes)
  >>>
  fmap GenStory

parseQuestFromNodes ∷ [Node] → Either String Quest
parseQuestFromNodes =
  mapM (parseContainer "event" parseEventFromNodes)
  >>>
  fmap GenQuest

parseEventFromNodes ∷ [Node] → Either String Event
parseEventFromNodes =
  mapM (parseContainer "p" parseParagraphFromNodes)
  >>>
  fmap (GenEvent <<< filter (not <<< nullOf folded))

parseParagraphFromNodes ∷ [Node] → Either String Paragraph
parseParagraphFromNodes = mapM parseParagraphChild >>> fmap mconcat
  where
    parseParagraphChild ∷ Node → Either String Paragraph
    parseParagraphChild (NodeInstruction _) = fail "unexpected XML instruction"
    parseParagraphChild (NodeComment _) = return mempty
    parseParagraphChild (NodeContent t) = return $ Text_ t
    parseParagraphChild (NodeElement (Element (Name tag _ _) attrs children)) =
      case lookup tag tags of
        Nothing → fail [i|unexpected tag <#{tag}>|]
        Just style
          | not <<< null $ attrs → fail [i|<#{tag}> had unexpected attributes|]
          | otherwise → Style style <$> parseParagraphFromNodes children
      where
        tags ∷ Map Text Style
        tags = mapFromList
          [ ("b", Bold)
          , ("u", Underline)
          , ("red", Color Red)
          , ("blue", Color Blue)
          , ("green", Color Green)
          , ("introduce", Introduce)
          ]

parseStoryFromDocument ∷ Document → Either String Story
parseStoryFromDocument =
  documentRoot
  >>>
  NodeElement
  >>>
  parseContainer "story" parseStoryFromNodes

parseStoryFromText ∷ LazyText.Text → Either String Story
parseStoryFromText = (parseText def >>> _Left %~ show) >=> parseStoryFromDocument

parseSubstitutions ∷ Paragraph → WriterT (Set Text) (Either String) SubParagraph
parseSubstitutions =
  replaceTextM parseSubstitutionsIn
  where
    parseSubstitutionsIn ∷ Text → WriterT (Set Text) (Either String) SubParagraph
    parseSubstitutionsIn chunk =
      runParserT parser () "" chunk
      >>=
      either
        (\msg → fail [i|Error parsing substitutions for text chunk "%{chunk}": #{msg}|])
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

data Gender = Male | Female deriving (Enum,Eq,Ord,Read,Show)

instance ToJSON Gender where
  toJSON gender = String $
    case gender of
      Male → "male"
      Female → "female"

instance FromJSON Gender where
  parseJSON = withText "expected text" parseGender
    where
      parseGender "male" = return Male
      parseGender "female" = return Female
      parseGender wrong = fail [i|gender must be "male" or "female", not "#{wrong}"|]

data Gendered = Gendered
  { _gendered_name ∷ Text
  , _gendered_gender ∷ Gender
  } deriving (Eq,Ord,Read,Show)
deriveJSONDropping 10 ''Gendered
makeLenses ''Gendered

_case ∷ Lens' Char Bool
_case = lens Char.isUpper (\c → bool (Char.toLower c) (Char.toUpper c))

first_case ∷ Traversal' Text Bool
first_case = _head . _case

findNounConverter ∷ Text → Maybe (Gender → Text)
findNounConverter "" = Nothing
findNounConverter word
  | word ∈ ["His", "Her"] = Just $ \case { Male → "His"; Female → "Her" }
  | otherwise =
      (\(m,f) gender →
        case gender of { Male → m; Female → f }
          |> (first_case .~ (word ^?! first_case))
          |> takeWhile (/= '|')
      )
      <$>
      find (elemOf both (word & first_case .~ False)) nouns
  where
    nouns =
      [ ("he","she")
      , ("him","her|obj")
      , ("his|pp","hers")
      , ("himself","herself")
      , ("his","her|pos")
      , ("man","woman")
      , ("son","daughter")
      ]

type Substitutor = Text → Either String Paragraph

substitute ∷ Substitutor → SubParagraph → Either String Paragraph
substitute substitutor = replaceTextM substituteIn
  where
    substituteIn (Literal t) = return $ Text_ t
    substituteIn (Key key) = substitutor key

isVowel ∷ Char → Bool
isVowel = (∈ "aeiouAEIOU")

makeSubstitutor ∷ (Text → Maybe Gendered) → (Text → Maybe Text) → Substitutor
makeSubstitutor _ _ "" = error "empty keys are not supported"
makeSubstitutor lookupGendered lookupNeutered key =
  key
    |> runParser parser () ""
    |> bimap show Text_
  where
    parser =
      (do starts_with_uppercase ← try $ do
            starts_with_uppercase ← Char.isUpper <$> oneOf "Aa"
            _ ← optional (char 'n')
            _ ← space
            return starts_with_uppercase
          neutered_name ← (rewords >>> pack) <$> many1 letter
          name ←
            maybe
              (fail [i|Unable to find neuter entity with name "#{neutered_name}".|])
              return
            $
            lookupNeutered neutered_name
          return $ mconcat
            [ if starts_with_uppercase then "A" else "a"
            , if fromMaybe False (isVowel <$> name ^? _head) then "n " else " "
            , name
            ]
      )
      <|>
      (do word ← pack <$> many1 (letter <|> char '|')
          maybe_name ←
            optionMaybe
            $
            between
              (char '[')
              (char ']')
              ((rewords >>> pack) <$> many (letter <|> space))
          case findNounConverter word of
            Nothing →
              maybe (fail [i|unrecognized word "#{word}"|]) return
              $
              (view gendered_name <$> lookupGendered word) <|> lookupNeutered word
            Just convertNoun → do
              let tryName name message =
                    lookupGendered name
                    |>
                    maybe
                      (fail message)
                      (
                        view gendered_gender
                        >>>
                        convertNoun
                        >>>
                        return
                      )
              case maybe_name of
                Nothing →
                  tryName "" "no default entity provided"
                Just name →
                  tryName name [i|unable to find entity with name "#{name}"|]
      )

clearNullElements ∷ (Wrapped s, Unwrapped s ~ [t]) ⇒ (t → Bool) → s → s
clearNullElements isNull = _Wrapped' %~ filter (not <<< isNull)

dropEmptyThingsFromStory ∷ GenText α ⇒ GenStory α → GenStory α
dropEmptyThingsFromStory =
  quests . events %~ clearNullElements (all textIsAllSpaces)
  >>>
  quests %~ clearNullElements (nullOf paragraphs)
  >>>
  clearNullElements (nullOf events)

parseQuote ∷ String → [SubEvent]
parseQuote =
  insertMarkers
  >>>
  LazyText.pack
  >>>
  (
    parseStoryFromText
    >=>
    (dropEmptyThingsFromStory >>> pure)
    >=>
    (
      traverseOf (quests . events . paragraphs) parseSubstitutions
      >>>
      runWriterT
      >>>
      fmap fst
    )
    >=>
    (
      dropEmptyThingsFromStory
      >>>
      \case
        GenStory [quest] → return $ unwrapGenQuest quest
        GenStory xs → fail [i|saw #{olength xs} quests instead of 1|]
    )
  )
  >>>
  either (show >>> error) identity

s = QuasiQuoter
  (parseQuote >>> Lift.lift)
  (error "Cannot use s as a pattern")
  (error "Cannot use s as a type")
  (error "Cannot use s as a dec")

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
