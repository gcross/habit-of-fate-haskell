import EveryDict exposing (EveryDict)

import Html exposing (..)

import Api exposing (..)

import Page.Account
import Page.Habits


type Model =
    AccountModel Page.Account.Model
  | HabitsModel Page.Habits.Model


type Msg =
    AccountMsg Page.Account.Msg
  | HabitsMsg Page.Habits.Msg


init : ( Model, Cmd Msg )
init =
  ( AccountModel
      { login_information={username="", password=""}
      , error=""
      }
  , Cmd.none
  )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model) of
    (AccountMsg account_msg, AccountModel account_model) ->
      case Page.Account.update account_msg account_model of
        Page.Account.LoginInProgress new_account_model account_cmd ->
          (AccountModel new_account_model, Cmd.map AccountMsg account_cmd)
        Page.Account.LoginFinished token ->
          ( HabitsModel { token=token, habits=EveryDict.empty, error="" }
          , Cmd.map HabitsMsg (getHabitsCmd token)
          )
    (HabitsMsg habit_msg, HabitsModel habit_model) ->
      let (new_habit_model, habit_cmd) = Page.Habits.update habit_msg habit_model
      in (HabitsModel new_habit_model, Cmd.map HabitsMsg habit_cmd)
    _ -> (model, Cmd.none)


view : Model -> Html Msg
view model =
  case model of
    AccountModel account_model -> Html.map AccountMsg (Page.Account.view account_model)
    HabitsModel habits_model -> Html.map HabitsMsg (Page.Habits.view habits_model)


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


main : Program Never Model Msg
main =
  program
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }
