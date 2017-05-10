module Page.Account exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Api exposing (..)

type alias Model =
  { login_information: { username: String, password: String }
  , error: String
  }


type Msg =
    Username String
  | Password String
  | CreateAccountRequest
  | CreateAccountResponse (ApiResult CreateAccountResult)
  | LoginRequest
  | LoginResponse (ApiResult LoginResult)


modifyLoginInformation :
  (LoginInformation -> LoginInformation) -> Model -> Model
modifyLoginInformation modify model =
  { model | login_information = modify model.login_information }


type UpdateAccountResult =
    LoginInProgress Model (Cmd Msg)
  | LoginFinished Token


update : Msg -> Model -> UpdateAccountResult
update msg model =
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


view : Model -> Html Msg
view model =
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
