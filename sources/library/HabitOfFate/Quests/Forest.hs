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

--------------------------------------------------------------------------------
----------------------------------- Imports ------------------------------------
--------------------------------------------------------------------------------

import Control.Lens
import Control.Monad.State hiding (State)
import Data.Bool
import Data.Map (fromList)
import Data.String.QQ

import HabitOfFate.Game
import HabitOfFate.Quest
import HabitOfFate.Substitution
import HabitOfFate.TH
import HabitOfFate.Trial
import HabitOfFate.Unicode
import Debug.Trace

--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------

data State = State
  { _healer ∷ Character
  , _patient ∷ Character
  , _herb ∷ String
  , _herb_found ∷ Bool
  , _credits_until_success ∷ Double
  , _credits_until_failure ∷ Double
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''State
makeLenses ''State

type ForestAction = QuestAction State

--------------------------------------------------------------------------------
-------------------------------- Text Functions --------------------------------
--------------------------------------------------------------------------------

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

forestText = textWithDefaultSubstitutionsForLens quest

--------------------------------------------------------------------------------
------------------------------------ Logic -------------------------------------
--------------------------------------------------------------------------------

run ∷ ForestAction ()
run = do
  spendCredits
    (quest . credits_until_failure)
    (game . failure_credits)
    >>=
    \case
      SomethingHappened → lost
      NothingHappened → averted
      NoCredits → return ()
  spendCredits
    (quest . credits_until_success)
    (game . success_credits)
    >>=
    \case
      SomethingHappened → (use $ quest . herb_found) >>= bool found won
      NothingHappened → wander
      NoCredits → return ()

--------------------------------------------------------------------------------
------------------------------------ Intro -------------------------------------
--------------------------------------------------------------------------------

introText = textWithDefaultSubstitutionsForLens id

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
  execStateT (introText [s|
    The last thing in the world that {Susie} wanted to do was to wander around
    alone in the forest this night, but {Tommy} was sick and would not live
    through the night unless {Susie} could find {an Illsbane} plant to brew
    medicine for {him|Tommy}. It is a nearly hopeless task, but she has no other
    choice.

    She prays for a micale, and then begins her search, carrying a candle to
    light the way.
  |])

--------------------------------------------------------------------------------
------------------------------------ Found -------------------------------------
--------------------------------------------------------------------------------

found ∷ ForestAction ()
found = do
  uniformAction ∘ map forestText $ found_texts
  quest . herb_found .= True
  numberUntilEvent 5 >>= (quest . credits_until_success .=)
  where
    found_texts =
      [[s|
After searching for what feels like hours, {Susie} nearly steps on {an Illsbane}
plant. {Her} heart leaps and {she} gives a short prayer of thanks. {She} reaches
down carefully to pick it up, and then starts heading back to her home.
       |]
      ,[s|
{Susie} starts to hear a sound and she can't tell whether it is a buzzing or the
most beautiful music she has ever heard. As it gets louder she notices the area
around her starting to get brighter. She looks around and sees a fairy, glowing
brightly in the dark. The fairy beckons to her, and then flies away. {Susie}
hesitates briefly, and then runs after the fairy.

{Susie} chases the fairy for about an hour, starting to doubt whether this is
such a good idea, when the fairy stops. She catches up to it and sees {an
Illsbane} plant under it. Carefully, she reaches down and picks it. When she
looks up, the fairy is gone.

{She} falls to her knees and thanks you for guiding her to the plan. {She} then
gets up and starts heading back to her home.
      |]
      ]

--------------------------------------------------------------------------------
------------------------------------- Won --------------------------------------
--------------------------------------------------------------------------------

won ∷ ForestAction ()
won = do
  forestText won_text
  game . belief += 1
  questHasEnded
  where
    won_text =
      [s|
{Susie} is starting to feel like {she} will never make it back when she notices
that things are starting to get brighter -- {she} must be getting close to the
vilage! {She} gives you thanks for guiding {her} home.

A little bit further, and {she} is back to her house. Without a moment to spare,
{she} immediately starts brewing medicine for {Tommy}! {She} brings the medicine
to {Tommy}, and wakes him up long enough to ladel it down his throat. {He|Tommy}
immediately falls back asleep. As {Susie} is filled with relief, exhaustion
catches up to her and {she} falls asleep on the floor.

{She} sleeps peacefully, with a smile on her face. The next day, she builds an
alter to you out of gratitude.
      |]

--------------------------------------------------------------------------------
------------------------------------- Lost -------------------------------------
--------------------------------------------------------------------------------

lost ∷ ForestAction ()
lost = do
  forestText lost_text
  game . belief -= 1
  questHasEnded
  where
    lost_text =
      [s|
{She} takes too long, and {Tommy} dies}. She prays to you asking what she did
wrong for you to abandon her in her time of need. She still believes in you, but
a little less than before.
      |]

--------------------------------------------------------------------------------
------------------------------------ Wander ------------------------------------
--------------------------------------------------------------------------------

wander ∷ ForestAction ()
wander = forestText [s|{She} wanders through the forest.|]

--------------------------------------------------------------------------------
----------------------------------- Averted ------------------------------------
--------------------------------------------------------------------------------

averted ∷ ForestAction ()
averted = forestText [s|{She} trips and falls, but gets up after minute.|]
