module Habit exposing (..)


import EveryDict exposing (EveryDict)
import Uuid exposing (Uuid)


type alias Credits = { success: Float, failure: Float }
type Scale = VeryLow | Low | Medium | High | VeryHigh
type alias Habit = { name: String, importance: Scale, difficulty: Scale }
type alias Habits = EveryDict Uuid Habit
