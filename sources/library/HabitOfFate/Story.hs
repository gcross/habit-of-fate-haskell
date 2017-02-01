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

instance IsString Paragraph where
  fromString = Text_ ∘ pack

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
  mappend (Merged xs) (Merged ys) = Merged (xs ⊕ ys)
  mappend (Merged xs) y = Merged (xs |> y)
  mappend x (Merged ys) = Merged (x <| ys)
  mappend x y = Merged ∘ fromList $ [x,y]

textFromParagraph ∷ Paragraph → Text
textFromParagraph = fold

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
          fail
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
      key ←
        pack
        <$>
        between
          (char '{')
          (char '}')
          (pack ∘ rewords <$> (many1 $ letter <|> char '|' <|> space))
      lift ∘ tell $ singletonSet key
      return ∘ Text_ ∘ Key $ key

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
      parseGender t = fail $
        printf "gender must be \"male\" or \"female\", not \"%s\"" t

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
        (first_case .~ (word ^?! first_case))
        ∘
        takeWhile (/= '|')
        $
        case gender of { Male → m; Female → f }
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
  bimap show Text_
  ∘
  runParser parser () ""
  $
  key
  where
    parser =
      (do starts_with_uppercase ← try $ do
            starts_with_uppercase ← Char.isUpper <$> oneOf "Aa"
            _ ← optional (char 'n')
            _ ← space
            return starts_with_uppercase
          neutered_name ← pack ∘ rewords <$> many1 letter
          name ←
            maybe
              (fail $ printf "unable to find neuter entity with name \"%s\"" neutered_name)
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
              (pack ∘ rewords <$> many (letter <|> space))
          case findNounConverter word of
            Nothing →
              maybe (fail $ printf "unrecognized word \"%s\"" word)
                    return
              $
              (view gendered_name <$> lookupGendered word) <|> lookupNeutered word
            Just convertNoun → do
              let tryName name message =
                    maybe
                      (fail message)
                      (
                        return
                        ∘
                        convertNoun
                        ∘
                        view gendered_gender
                      )
                    $
                    lookupGendered name
              case maybe_name of
                Nothing →
                  tryName "" "no default entity provided"
                Just name →
                  tryName name (printf "unable to find entity with name \"%s\"" name)
      )

clearNullElements ∷ (Wrapped s, Unwrapped s ~ [t]) ⇒ (t → Bool) → s → s
clearNullElements isNull = _Wrapped' %~ filter (not ∘ isNull)

dropEmptyThingsFromStory ∷ GenText α ⇒ GenStory α → GenStory α
dropEmptyThingsFromStory =
  (clearNullElements (nullOf events))
  ∘
  (quests %~ clearNullElements (nullOf paragraphs))
  ∘
  (quests . events %~ clearNullElements (all textIsAllSpaces))

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
        GenStory xs → fail $ printf "saw %i quests instead of 1" (length xs)
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
    recurse (Text_ t) = [NodeContent t]

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
