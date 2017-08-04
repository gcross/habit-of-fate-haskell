module Page.Habits exposing (..)


import EveryDict exposing (EveryDict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Monocle.Lens exposing (..)


import Api exposing (..)
import Habit exposing (..)


type Msg =
    AddHabit


type alias Model = List Habit


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddHabit -> (model, Cmd.none)


view : Model -> Html Msg
view model =
  div []
    [ ul [] (List.map (\habit -> li [] [text habit.name]) model)
    , button [ onClick AddHabit ] []
    ]
