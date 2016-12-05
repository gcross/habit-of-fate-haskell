{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quests.Forest where

import Control.Lens
import Control.Monad.State hiding (State)
import Data.Map (fromList)
import Data.String.QQ

import HabitOfFate.Game
import HabitOfFate.Quest
import HabitOfFate.Substitution
import HabitOfFate.TH
import HabitOfFate.Trial
import HabitOfFate.Unicode
import Debug.Trace

data State = State
  { _healer ∷ Character
  , _patient ∷ Character
  , _herb ∷ String
  , _found ∷ Bool
  , _credits_until_success ∷ Double
  , _credits_until_failure ∷ Double
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''State
makeLenses ''State

type ForestAction = QuestAction State

defaultSubstitutionTable ∷ State → Substitutions
defaultSubstitutionTable forest = makeSubstitutionTable
  [("Susie",forest ^. healer)
  ,("Tommy",forest ^. patient)
  ,("Illsbane",Character (forest ^. herb) Neuter)
  ]

textWithDefaultSubstitutionsPlus ∷ MonadGame m ⇒ State → [(String,String)] → String → m ()
textWithDefaultSubstitutionsPlus forest additional_substitutions template =
  text
  ∘
  flip substitute template
  ∘
  (⊕ fromList additional_substitutions)
  ∘
  defaultSubstitutionTable
  $
  forest

textWithDefaultSubstitutionsForLens ∷ (MonadState s m, MonadGame m) ⇒ Lens' s State → String → m ()
textWithDefaultSubstitutionsForLens lens template =
  use lens
  >>=
  \forest → textWithDefaultSubstitutionsPlus forest [] template

textWithDefaultSubstitutionsIntro = textWithDefaultSubstitutionsForLens id
textWithDefaultSubstitutions =      textWithDefaultSubstitutionsForLens quest

new ∷ Game State
new =
  (
    State
      (Character "Susie" Female)
      (Character "Tommy" Male)
      "Illsbane"
      False
    <$> numberUntilEvent 5
    <*> numberUntilEvent 1
  )
  >>=
  execStateT introText

run ∷ ForestAction ()
run = do
  spendCredits
    (quest . credits_until_failure)
    (game . failure_credits)
    >>=
    \case
      SomethingHappened → do
        loseText
        game . belief -= 1
        questHasEnded
      NothingHappened → avertedText
      NoCredits → return ()
  spendCredits
    (quest . credits_until_success)
    (game . success_credits)
    >>=
    \case
      SomethingHappened → do
        herb_found ← use $ quest . found
        if herb_found
          then do
            winText
            game . belief += 1
            questHasEnded
          else do
            foundText
            quest . found .= True
            numberUntilEvent 5 >>= (quest . credits_until_success .=)
      NothingHappened → wanderText
      NoCredits → return ()

avertedText = textWithDefaultSubstitutions [s|
  {She} trips and falls, but gets up after minute.
|]

foundText = textWithDefaultSubstitutions [s|
  After wandering for what feels like hours, {Susie} nearly steps on {an
  Illsbane} plant. {Her} heart leaps and {she} gives a short prayer of thanks.
  {She} reaches down carefully to pick it.

  Now {she} just needs to find {her} way back to the village, but she is hopeful
  -- you have guided {her} this far, after all!
|]

introText = textWithDefaultSubstitutionsIntro [s|
  The last thing in the world that {Susie} wanted to do was to wander around
  alone in the forest this night, but {Tommy} was sick and would not live
  through the night unless {Susie} could find {an Illsbane} plant to brew
  medicine for {him|Tommy}.

  She begins her search.|]

loseText = textWithDefaultSubstitutions [s|
  {She} takes too long, and {Tommy} dies}.  She prays to you asking what she did wrong for you to abandon her in her time of need.  She still believes in you, but a little less than before.
|]

stumbleText = textWithDefaultSubstitutions [s|
  {She} stumbles around in the dark.
|]

wanderText = textWithDefaultSubstitutions [s|
  {She} wander through the forest.
|]

winText = textWithDefaultSubstitutions [s|
  {Susie} is starting to feel like {she} will never make it back when she
  notices that things are starting to get brighter -- {she} must be getting
  close to the vilage! {She} gives you thanks for guiding {her} home.

  A little bit further, and {she} is back to her house. Without a moment to
  spare, {she} immediately starts brewing medicine for {Tommy}! {She} brings the
  medicine to {Tommy}, and wakes him up long enough to ladel it down his throat.
  {He|Tommy} immediately falls back asleep. As {Susie} is filled with relief,
  exhaustion catches up to her and {she} falls asleep on the floor.

  {She} sleeps peacefully, with a smile on her face. The next day, she builds an
  alter to you out of gratitude.

  Well done!
|]
