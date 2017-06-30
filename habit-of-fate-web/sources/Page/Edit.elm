module Page.Edit exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
  { token: Token
  , uuid: Uuid
  , habit: Habit
  , error: String
  }


type Msg =
    Name String
  | Difficulty String
  | Importance String
  | Cancel
  | Response (ApiResult CreateAccountResult)


type EditResult =
    InProgress Model (Cmd Msg)
  | Finished


errorsIn : Model -> [String]


update : Msg -> Model -> EditResult
update msg model =
  case msg of
    Name name ->
      InProgress
        (modifyHabit (\habit -> { habit | name=name }) model)
        Cmd.none
    Credits credits ->
      InProgress
        (modifyHabit (\habit -> { habit | credits=credits }) model)
        Cmd.none
    Put ->
      InProgress
        model
        (Cmd.map Response (putHabitCmd model.token model.uuid model.habit))
    Response ->
      case response of
        ExpectedResult _ -> Finished
        UnexpectedError error ->
          InProgress { model | error=toString error } Cmd.none


view : Model -> Html Msg
view model =
  div []
    [ table []
        (
          [ tr []
              [ td [] [text "Name:"]
              , td [] [input [type_ "text", onInput Name] []]
              ]
          , tr []
              [ td [] [text "Difficulty:"]
              , td [] [input [type_ "number", onInput Difficulty] []]
              ]
          , tr []
              [ td [] [text "Importance:"]
              , td [] [input [type_ "number", onInput Importance] []]
              ]
          ]
    , div [] [ button [onClick Request] [text "Submit"] ]
    , div [style [("color", "red")]] [ul [] (map li (errorsIn model))]
    ]
