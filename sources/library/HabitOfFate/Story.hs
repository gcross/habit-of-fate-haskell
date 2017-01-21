{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Story where

import HabitOfFate.Prelude

import qualified Data.Char as Char
import Data.Default
import qualified Data.Text.Lazy as LazyText
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (Lift)
import qualified Language.Haskell.TH.Lift as Lift
import Language.Haskell.TH.Quote
import Text.XML
import Text.Parsec

import HabitOfFate.TH

data Color = Red | Green | Blue deriving (Enum,Eq,Lift,Ord,Read,Show)

data Style = Bold | Underline | Color Color deriving (Eq,Lift,Ord,Read,Show)

data GenParagraph α =
    Style Style (GenParagraph α)
  | Merged (Seq (GenParagraph α))
  | Text_ α
  deriving (Eq,Foldable,Functor,Lift,Ord,Read,Show,Traversable)

replaceTextM ∷ Applicative f ⇒ (α → f (GenParagraph β)) → GenParagraph α → f (GenParagraph β)
replaceTextM f (Style s x) = Style s <$> replaceTextM f x
replaceTextM f (Merged xs) = Merged <$> traverse (replaceTextM f) xs
replaceTextM f (Text_ t) = f t

instance Monoid (GenParagraph α) where
  mempty = Merged mempty
  mappend (Merged xs) (Merged ys) = Merged (xs ⊕ ys)
  mappend (Merged xs) y = Merged (xs |> y)
  mappend x (Merged ys) = Merged (x <| ys)
  mappend x y = mconcat [x,y]
  mconcat = Merged ∘ fromList

type Paragraph = GenParagraph Text

data SubText = Key Text | Literal Text deriving (Eq,Lift,Ord,Read,Show)
type SubParagraph = GenParagraph SubText

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

isNull ∷ Paragraph → Bool
isNull (Text_ t) = allSpaces t
isNull (Merged xs) = all isNull xs
isNull _ = False

parseStory ∷ Document → Either String [[[Paragraph]]]
parseStory =
  parseContainer "story" parseQuests
  ∘
  NodeElement
  ∘
  documentRoot
  where
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

    parseQuests ∷ [Node] → Either String [[[Paragraph]]]
    parseQuests =
      fmap (filter (not ∘ null))
      ∘
      mapM (parseContainer "quest" parseEvents)

    parseEvents ∷ [Node] → Either String [[Paragraph]]
    parseEvents =
      fmap (filter (not ∘ null))
      ∘
      mapM (parseContainer "event" parseParagraphs)

    parseParagraphs ∷ [Node] → Either String [Paragraph]
    parseParagraphs =
      fmap (filter (not ∘ isNull))
      ∘
      mapM (parseContainer "p" parseGenParagraph)

    parseGenParagraph ∷ [Node] → Either String Paragraph
    parseGenParagraph = fmap mconcat ∘ mapM parseGenParagraphChild

    parseGenParagraphChild ∷ Node → Either String Paragraph
    parseGenParagraphChild (NodeInstruction _) = fail "unexpected XML instruction"
    parseGenParagraphChild (NodeComment _) = return mempty
    parseGenParagraphChild (NodeContent t) = return $ Text_ t
    parseGenParagraphChild (NodeElement (Element (Name tag _ _) attrs children)) =
      case tag of
        "b"
          | not ∘ null $ attrs → fail "<b> had unexpected attributes"
          | otherwise → Style Bold <$> parseGenParagraph children
        "u"
          | not ∘ null $ attrs → fail "<u> tag had unexpected attributes"
          | otherwise → Style Underline <$> parseGenParagraph children
        "color" → case mapToList attrs of
          [("hue",hue)] → case hue of
            "red" → Style (Color Red) <$> parseGenParagraph children
            "blue" → Style (Color Blue) <$> parseGenParagraph children
            "green" → Style (Color Green) <$> parseGenParagraph children
            _ → fail $ printf "invalid hue %s" hue
          _ → fail "<color> must have just a hue attribute"
        _ → fail $ printf "unexpected tag <%s>" tag

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

    takeTillNextSub = Text_ ∘ Literal ∘ (^. packed) <$> many (satisfy (/='{'))

    parseAnotherSub = do
      char '{'
      key ← (^. packed) ∘ unwords ∘ words <$> many1 (satisfy (/='}'))
      when (elemOf text '{' key) $ fail "nested brace"
      char '}'
      lift ∘ tell $ singletonSet key
      return ∘ Text_ ∘ Key $ key

type Substitutions = Map Text Text

substitute ∷ Substitutions → SubParagraph → Either (Set Text) Paragraph
substitute subs = traverse substituteIn
  where
    substituteIn ∷ SubText → Either (Set Text) Text
    substituteIn (Literal t) = return t
    substituteIn (Key k) =
      case lookup k subs of
        Nothing → throwError ∘ singletonSet $ k
        Just t → return t

data Gender = Male | Female | Neuter deriving (Eq,Ord,Read,Show)
deriveJSON ''Gender

data Character = Character Text Gender deriving (Eq,Ord,Read,Show)
deriveJSON ''Character

makeSubstitutionTable ∷ [(Text,Character)] → Substitutions
makeSubstitutionTable [] = mempty
makeSubstitutionTable table@((_,first_character@(Character _ _)):_) =
    mapFromList
    $
    makeNouns first_character
    ⊕
    concatMap
      (\(key, character@(Character name _)) →
          (name, name)
          :
          makeArticles key character ⊕ fmap (_1 ⊕~ ('|' <| key)) (makeNouns character)
      )
      table
  where
    makeArticles ∷ Text → Character → [(Text,Text)]
    makeArticles key (Character name _) =
        [("a " ⊕ key, articleValue False)
        ,("A " ⊕ key, articleValue True)
        ,("an " ⊕ key, articleValue False)
        ,("An " ⊕ key, articleValue True)
        ,("the " ⊕ key, "the " ⊕ name)
        ,("The " ⊕ key, "The " ⊕ name)
        ]
      where
        articleValue ∷ 𝔹 → Text
        articleValue capitalize = article ⊕ " " ⊕ name
          where
            article =
              (_head %~ if capitalize then Char.toUpper else Char.toLower)
              $
              case name ^? _head of
                Just c | Char.toLower c ∈ "aeiou" → "an"
                _ → "a"

    makeNouns ∷ Character → [(Text,Text)]
    makeNouns (Character _ gender) = concat
        [subject_pronouns
        ,object_pronouns
        ,possessive_prononuns
        ,descriptive_possessive_pronouns
        ,category_nouns
        ]
      where
        capitalized = (_head %~ Char.toUpper)

        subject_pronouns =
            fmap (,pronoun) ["he","she","it"]
            ⊕
            fmap (,capitalized pronoun) ["He","She","It"]
          where
            pronoun = case gender of
                Male → "he"
                Female → "she"
                Neuter → "it"

        object_pronouns = fmap (,pronoun) ["him","her","it"]
          where
            pronoun = case gender of
                Male → "him"
                Female → "her"
                Neuter → "it"

        possessive_prononuns =
            fmap (,pronoun) ["his","her'","its"]
            ⊕
            fmap (,capitalized pronoun) ["His","Her","Its"]
          where
            pronoun = case gender of
                Male → "his"
                Female → "her"
                Neuter → "its"

        descriptive_possessive_pronouns =
            fmap (,pronoun) ["his","hers","its"]
          where
            pronoun = case gender of
                Male → "his"
                Female → "her"
                Neuter → "its"

        category_nouns =
            fmap (,category) ["man","woman","thing"]
          where
            category = case gender of
                Male → "man"
                Female → "woman"
                Neuter → "thing"

parseQuote ∷ String → [[SubParagraph]]
parseQuote =
    either (error ∘ show) identity
    ∘
    (
      (_Left %~ show) ∘ parseText def ∘ LazyText.pack
      >=>
      parseStory
      >=>
      (
        fmap fst
        ∘
        runWriterT
        ∘
        traverseOf (each . each . each) parseSubstitutions
      )
      >=>
      (\case
        [x] → return x
        xs → throwError $ printf "saw %i quests instead of 1" (length xs)
      )
    )
    ∘
    insertMarkers

s = QuasiQuoter
  (Lift.lift ∘ parseQuote)
  (error "Cannot use s as a pattern")
  (error "Cannot use s as a type")
  (error "Cannot use s as a dec")

s1 = QuasiQuoter
  (
    (\case
      [x] → [|x|]
      xs → error $ printf "saw %i events instead of 1" (length xs)
    )
    ∘
    parseQuote
  )
  (error "Cannot use s1 as a pattern")
  (error "Cannot use s1 as a type")
  (error "Cannot use s1 as a dec")

generateParagraph ∷ Paragraph → [Node]
generateParagraph (Style style paragraph) =
  singleton
  $
  case style of
    Bold → NodeElement $ Element "b" mempty nested
    Underline → NodeElement $ Element "u" mempty nested
    Color color →
      let color_name = case color of
            Red → "red"
            Blue → "blue"
            Green → "green"
      in NodeElement $ Element "color" (singletonMap "hue" color_name) nested
  where
    nested = generateParagraph paragraph

generateParagraph (Merged children) = concatMap generateParagraph children
generateParagraph (Text_ t) = [NodeContent t]

generateEvent ∷ [Paragraph] → Node
generateEvent = NodeElement ∘ Element "event" mempty ∘ concatMap generateParagraph

generateQuest ∷ [[Paragraph]] → Node
generateQuest = NodeElement ∘ Element "quest" mempty ∘ foldr ((:) ∘ generateEvent) []

generateStory ∷ [[[Paragraph]]] → Document
generateStory =
  (\n → Document (Prologue [] Nothing []) n [])
  ∘
  Element "story" mempty
  ∘
  foldr ((:) ∘ generateQuest) []

renderStory ∷ [[[Paragraph]]] → LazyText.Text
renderStory = renderText def ∘ generateStory
