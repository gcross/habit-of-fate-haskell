import EveryDict exposing (EveryDict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


import Api exposing (..)


--------------------------------------------------------------------------------
----------------------------------- Account ------------------------------------
--------------------------------------------------------------------------------


type alias AccountModelType =
  { login_information: { username: String, password: String }
  , error: String
  }


type AccountMsg =
    Username String
  | Password String
  | CreateAccountRequest
  | CreateAccountResponse (ApiResult CreateAccountResult)
  | LoginRequest
  | LoginResponse (ApiResult LoginResult)


modifyLoginInformation :
  (LoginInformation -> LoginInformation) -> AccountModelType -> AccountModelType
modifyLoginInformation modify model =
  { model | login_information = modify model.login_information }


type UpdateAccountResult =
    LoginInProgress AccountModelType (Cmd AccountMsg)
  | LoginFinished Token


updateAccount : AccountMsg -> AccountModelType -> UpdateAccountResult
updateAccount msg model =
  case msg of
    Username username ->
      LoginInProgress
        (modifyLoginInformation (\login -> { login | username=username }) model)
        Cmd.none
    Password password ->
      LoginInProgress
        (modifyLoginInformation (\login -> { login | password=password }) model)
        Cmd.none
    CreateAccountRequest ->
      LoginInProgress
        model
        (Cmd.map CreateAccountResponse (createAccountCmd model.login_information))
    CreateAccountResponse response ->
      case response of
        ExpectedResult (AccountCreated token) -> LoginFinished token
        ExpectedResult AccountAlreadyExists ->
          LoginInProgress { model | error="Account already exists!" } Cmd.none
        UnexpectedError error ->
          LoginInProgress { model | error=toString error} Cmd.none
    LoginRequest ->
      LoginInProgress
        model
        (Cmd.map LoginResponse (loginCmd model.login_information))
    LoginResponse response ->
      case response of
        ExpectedResult (LoginSuccessful token) -> LoginFinished token
        ExpectedResult NoSuchAccount ->
          LoginInProgress { model | error="No such account" } Cmd.none
        ExpectedResult InvalidPassword ->
          LoginInProgress { model | error="Invalid password." } Cmd.none
        UnexpectedError error ->
          LoginInProgress { model | error=toString error } Cmd.none


viewAccount : AccountModelType -> Html AccountMsg
viewAccount model =
  div []
    [ table []
        [ tr []
            [ td [] [text "Username:"]
            , td [] [input [type_ "text", onInput Username] []]
            ]
        , tr []
            [ td [] [text "Password:"]
            , td [] [input [type_ "password", onInput Password] []]
            ]
        ]
    , div []
        [ button [onClick CreateAccountRequest] [text "Create account"]
        , button [onClick LoginRequest] [text "Login"]
        ]
    , div [style [("color", "red")]] [text model.error]
    ]


--------------------------------------------------------------------------------
------------------------------------ Habits ------------------------------------
--------------------------------------------------------------------------------


type alias HabitsMsg = ApiResult Habits


type alias HabitsModelType =
  { token: Token
  , habits: Habits
  , error: String
  }


updateHabits : HabitsMsg -> HabitsModelType -> (HabitsModelType, Cmd HabitsMsg)
updateHabits msg model =
  case msg of
    UnexpectedError error -> ({ model | error=toString error }, Cmd.none)
    ExpectedResult habits -> ({ model | habits=habits }, Cmd.none)


viewHabits : HabitsModelType -> Html HabitsMsg
viewHabits habits_model =
  div [] [ ul [] (List.map (\habit -> li [] [text habit.name]) (EveryDict.values habits_model.habits)) ]


--------------------------------------------------------------------------------
----------------------------------- Program ------------------------------------
--------------------------------------------------------------------------------


type Model =
    AccountModel AccountModelType
  | HabitsModel HabitsModelType


type Msg =
    AccountMsg AccountMsg
  | HabitsMsg HabitsMsg


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
      case updateAccount account_msg account_model of
        LoginInProgress new_account_model account_cmd ->
          (AccountModel new_account_model, Cmd.map AccountMsg account_cmd)
        LoginFinished token ->
          ( HabitsModel { token=token, habits=EveryDict.empty, error="" }
          , Cmd.map HabitsMsg (getHabitsCmd token)
          )
    (HabitsMsg habit_msg, HabitsModel habit_model) ->
      let (new_habit_model, habit_cmd) = updateHabits habit_msg habit_model
      in (HabitsModel new_habit_model, Cmd.map HabitsMsg habit_cmd)
    _ -> (model, Cmd.none)


view : Model -> Html Msg
view model =
  case model of
    AccountModel account_model -> Html.map AccountMsg (viewAccount account_model)
    HabitsModel habits_model -> Html.map HabitsMsg (viewHabits habits_model)


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
