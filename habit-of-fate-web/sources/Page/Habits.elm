module Page.Habits exposing (..)

import EveryDict exposing (EveryDict)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


import Api exposing (..)


type alias Msg = ApiResult Habits


type alias Model =
  { token: Token
  , habits: Habits
  , error: String
  }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UnexpectedError error -> ({ model | error=toString error }, Cmd.none)
    ExpectedResult habits -> ({ model | habits=habits }, Cmd.none)


view : Model -> Html Msg
view habits_model =
  div [] [ ul [] (List.map (\habit -> li [] [text habit.name]) (EveryDict.values habits_model.habits)) ]
