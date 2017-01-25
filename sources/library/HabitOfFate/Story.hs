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
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module HabitOfFate.Story where

import HabitOfFate.Prelude

import qualified Data.Char as Char
import qualified Data.Text.Lazy as LazyText
import Instances.TH.Lift ()
import GHC.Generics hiding (from)
import Labels ((:=))
import Language.Haskell.TH.Lift (Lift)
import qualified Language.Haskell.TH.Lift as Lift
import Language.Haskell.TH.Quote
import Rainbow
import Text.XML
import Text.Parsec hiding (uncons)

import HabitOfFate.TH

data Color = Red | Green | Blue deriving (Enum,Eq,Generic,Lift,Ord,Read,Show)

data Style = Bold | Underline | Color Color | Introduce deriving (Eq,Generic,Lift,Ord,Read,Show)

data GenParagraph α =
    Style Style (GenParagraph α)
  | Merged (Seq (GenParagraph α))
  | Text_ α
  deriving (Eq,Foldable,Functor,Generic,Lift,Ord,Read,Show,Traversable)

instance Monoid (GenParagraph α) where
  mempty = Merged mempty
  mappend (Merged xs) (Merged ys) = Merged (xs ⊕ ys)
  mappend (Merged xs) y = Merged (xs |> y)
  mappend x (Merged ys) = Merged (x <| ys)
  mappend x y = mconcat [x,y]
  mconcat = Merged ∘ fromList

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
createEvent = GenEvent ∘ toList

createQuest ∷ Foldable t ⇒ t Event → Quest
createQuest = GenQuest ∘ toList

createStory ∷ Foldable t ⇒ t Quest → Story
createStory = GenStory ∘ toList

eventToLists ∷ GenEvent α → [GenParagraph α]
eventToLists = unwrapGenEvent

questToLists ∷ GenQuest α → [[GenParagraph α]]
questToLists = fmap eventToLists ∘ unwrapGenQuest

storyToLists ∷ GenStory α → [[[GenParagraph α]]]
storyToLists = fmap questToLists ∘ unwrapGenStory

type Paragraph = GenParagraph Text
type Event = GenEvent Text
type Quest = GenQuest Text
type Story = GenStory Text

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

insertMarkers ∷ String → String
insertMarkers =
  unlines
  ∘
  (\x → ["<story><quest><event><p>"] ⊕ x ⊕ ["</p></event></quest></story>"])
  ∘
  fmap (\case
    "" → "</p><p>"
    '=':_ → "</p></event><event><p>"
    line → line
  )
  ∘
  fmap (dropWhile (∈ " \t"))
  ∘
  lines

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
          fail $ printf "expected <%s> but got <%s>" expected_tag tag
      | not ∘ null $ attrs →
          fail $ printf "expected no attributes in <%s>"  tag
      | otherwise → parseChildren children

parseStoryFromNodes ∷ [Node] → Either String Story
parseStoryFromNodes =
  fmap GenStory
  ∘
  mapM (parseContainer "quest" parseQuestFromNodes)

parseQuestFromNodes ∷ [Node] → Either String Quest
parseQuestFromNodes =
  fmap GenQuest
  ∘
  mapM (parseContainer "event" parseEventFromNodes)

parseEventFromNodes ∷ [Node] → Either String Event
parseEventFromNodes =
  fmap (GenEvent ∘ filter (not ∘ nullOf folded))
  ∘
  mapM (parseContainer "p" parseParagraphFromNodes)

parseParagraphFromNodes ∷ [Node] → Either String Paragraph
parseParagraphFromNodes = fmap mconcat ∘ mapM parseParagraphChild
  where
    parseParagraphChild ∷ Node → Either String Paragraph
    parseParagraphChild (NodeInstruction _) = fail "unexpected XML instruction"
    parseParagraphChild (NodeComment _) = return mempty
    parseParagraphChild (NodeContent t) = return $ Text_ t
    parseParagraphChild (NodeElement (Element (Name tag _ _) attrs children)) =
      case lookup tag tags of
        Nothing → fail $ printf "unexpected tag <%s>" tag
        Just style
          | not ∘ null $ attrs → fail $ printf "<%s> had unexpected attributes" tag
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
  parseContainer "story" parseStoryFromNodes
  ∘
  NodeElement
  ∘
  documentRoot

parseStoryFromText ∷ LazyText.Text → Either String Story
parseStoryFromText = (_Left %~ show) ∘ parseText def >=> parseStoryFromDocument

parseSubstitutions ∷ Paragraph → WriterT (Set Text) (Either String) SubParagraph
parseSubstitutions =
  replaceTextM parseSubstitutionsIn
  where
    parseSubstitutionsIn ∷ Text → WriterT (Set Text) (Either String) SubParagraph
    parseSubstitutionsIn t =
      runParserT parser () "" t
      >>=
      either
        (
          throwError
          ∘
          printf "Error parsing substitutions for text chunk \"%s\": %s" t
          ∘
          show
        )
        return

    parser =
      mappend
        <$> takeTillNextSub
        <*> (mconcat <$> (many $ mappend <$> parseAnotherSub <*> takeTillNextSub))

    takeTillNextSub = Text_ ∘ Literal ∘ pack <$> many (satisfy (/='{'))

    parseAnotherSub = do
      char '{'
      key ← pack ∘ unwords ∘ words <$> many1 (satisfy (/='}'))
      when (elemOf text '{' key) $ fail "nested brace"
      char '}'
      lift ∘ tell $ singletonSet key
      return ∘ Text_ ∘ Key $ key

type Substitutor = Text → Maybe Paragraph

substitute ∷ Substitutor → SubParagraph → Either String Paragraph
substitute lookupSubFor = replaceTextM substituteIn
  where
    substituteIn (Literal t) = return (Text_ t)
    substituteIn (Key key) =
      case lookupSubFor key of
        Nothing → throwError $ printf "unable to find key: %s" key
        Just value → return value

data Gender = Male | Female | Neuter deriving (Eq,Ord,Read,Show)
deriveJSON ''Gender

data Character = Character Text Gender deriving (Eq,Ord,Read,Show)
deriveJSON ''Character

makeSubstitutor ∷ [(Text,Character)] → Substitutor
makeSubstitutor characters key =
  (\(Character name _) → Text_ name)
  <$>
  lookup key characters

clearNullElements ∷ (Wrapped s, Unwrapped s ~ [t]) ⇒ (t → Bool) → s → s
clearNullElements isNull = _Wrapped' %~ filter (not ∘ isNull)

class TextIsNull α where
  textIsNull ∷ α → Bool

instance TextIsNull Text where
  textIsNull = allSpaces

instance TextIsNull SubText where
  textIsNull (Literal t) = allSpaces t
  textIsNull _ = False

dropEmptyThingsFromStory ∷ TextIsNull α ⇒ GenStory α → GenStory α
dropEmptyThingsFromStory =
  (clearNullElements (nullOf events))
  ∘
  (quests %~ clearNullElements (nullOf paragraphs))
  ∘
  (quests . events %~ clearNullElements (all textIsNull))

parseQuote ∷ String → [SubEvent]
parseQuote =
    either (error ∘ show) identity
    ∘
    (
      parseStoryFromText
      >=>
      (return ∘ dropEmptyThingsFromStory)
      >=>
      (
        fmap fst
        ∘
        runWriterT
        ∘
        traverseOf (quests . events . paragraphs) parseSubstitutions
      )
      >=>
      (\case
        GenStory [quest] → return $ unwrapGenQuest quest
        GenStory xs → throwError $ printf "saw %i quests instead of 1" (length xs)
      )
      ∘
      dropEmptyThingsFromStory
    )
    ∘
    LazyText.pack
    ∘
    insertMarkers

s = QuasiQuoter
  (Lift.lift ∘ parseQuote)
  (error "Cannot use s as a pattern")
  (error "Cannot use s as a type")
  (error "Cannot use s as a dec")

s_fixed = QuasiQuoter
  (
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
      xs → error $ printf "saw %i events, which is too many (> 10)" (length xs)
    )
    ∘
    parseQuote
  )
  (error "Cannot use s1 as a pattern")
  (error "Cannot use s1 as a type")
  (error "Cannot use s1 as a dec")

renderParagraphToNodes ∷ Paragraph → [Node]
renderParagraphToNodes paragraph =
  case recurse paragraph of
    [] → []
    nodes → [NodeElement $ Element "p" mempty nodes]
  where
    recurse ∷ Paragraph → [Node]
    recurse (Style style p)
      | null nested = []
      | otherwise =
          singleton
          ∘
          NodeElement
          $
          let tag = case style of
                Bold → "b"
                Underline → "u"
                Color Red → "red"
                Color Blue → "blue"
                Color Green → "green"
                Introduce → "introduce"
          in Element tag mempty nested
      where
        nested = recurse p
    recurse (Merged children) = concatMap recurse children
    recurse (Text_ t)
      | allSpaces t = []
      | otherwise = [NodeContent t]

renderEventToNode ∷ Event → Node
renderEventToNode =
  NodeElement
  ∘
  Element "event" mempty
  ∘
  concatMap renderParagraphToNodes
  ∘
  unwrapGenEvent

renderQuestToNode ∷ Quest → Node
renderQuestToNode =
  NodeElement
  ∘
  Element "quest" mempty
  ∘
  foldr ((:) ∘ renderEventToNode) []
  ∘
  unwrapGenQuest

renderStoryToDocument ∷ Story → Document
renderStoryToDocument =
  (\n → Document (Prologue [] Nothing []) n [])
  ∘
  Element "story" mempty
  ∘
  foldr ((:) ∘ renderQuestToNode) []
  ∘
  unwrapGenStory

renderStoryToText ∷ Story → LazyText.Text
renderStoryToText = renderText def ∘ renderStoryToDocument

renderStoryToChunks ∷ Story → [Chunk Text]
renderStoryToChunks =
  toList
  ∘
  execWriter
  ∘
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
  ∘
  uncons
  ∘
  unwrapGenStory
  ∘
  dropEmptyThingsFromStory
  where
    tellChunk ∷ MonadWriter (Seq (Chunk Text)) m ⇒ Chunk Text → m ()
    tellChunk = tell ∘ singleton

    tellLine ∷ MonadWriter (Seq (Chunk Text)) m ⇒ Text → m ()
    tellLine = tellChunk ∘ chunk ∘ (|> '\n')

    tellNewline ∷ MonadWriter (Seq (Chunk Text)) m ⇒ m ()
    tellNewline = tellLine ""

    tellSeparator ∷ MonadWriter (Seq (Chunk Text)) m ⇒ Char → m ()
    tellSeparator = tellLine ∘ replicate 80

    tellEventSeparator = tellSeparator '―'
    tellQuestSeparator = tellSeparator '═'

    renderQuest = go ∘ unwrapGenQuest
      where
        go [] = return ()
        go (x:[]) = renderEvent x
        go (x:xs) = do
          renderEvent x
          tellEventSeparator
          go xs

    renderEvent = go ∘ unwrapGenEvent
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
      flip evalStateT
        ( #number_of_columns := 0
        , #current_word := (mempty ∷ Seq Char)
        , #saw_spaces_last := False
        , #pending_chunks := (mempty ∷ Seq (Chunk Text))
        , #pending_length := 0
        )
      ∘
      go mempty
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
                saw_spaces_last ← l_ #saw_spaces_last <<.= True
                current_word_is_empty ←
                  (&&)
                    <$> (null <$> use (l_ #current_word))
                    <*> (null <$> use (l_ #pending_chunks))
                unless (saw_spaces_last || current_word_is_empty) $ do
                  word ← repack <$> (l_ #current_word <<.= mempty)
                  word_length ← (length word +) <$> (l_ #pending_length <<.= 0)
                  number_of_columns ← use (l_ #number_of_columns)
                  case number_of_columns of
                    0 → l_ #number_of_columns .= word_length
                    _ | number_of_columns + 1 + word_length >= 80 → do
                          tellNewline
                          l_ #number_of_columns .= word_length
                    _ | otherwise → do
                          tellChunk $ chunk " "
                          l_ #number_of_columns += 1 + word_length
                  (l_ #pending_chunks <<.= mempty) >>= traverse_ tellChunk
                  tellChunk $ formatting ⊕ chunk word
              else do
                l_ #current_word %= (|> c)
                l_ #saw_spaces_last .= False
          )
          >>
          (do
            word ← repack <$> (l_ #current_word <<.= mempty)
            l_ #pending_chunks %= (|> formatting ⊕ chunk word)
            l_ #pending_length += length word
          )
