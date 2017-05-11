module Page.Account exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Api exposing (..)

type alias Model =
  { login_information: { username: String, password: String }
  , maybe_password2: Maybe String
  , error: String
  }


type Msg =
    Username String
  | Password String
  | Password2 String
  | Request
  | CreateAccountMode
  | CreateAccountResponse (ApiResult CreateAccountResult)
  | LoginMode
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
    Password2 password2 ->
      LoginInProgress
        { model | maybe_password2=Just password2 }
        Cmd.none
    Request ->
      case model.maybe_password2 of
        Nothing ->
          LoginInProgress
            model
            (Cmd.map LoginResponse (loginCmd model.login_information))
        Just password2 ->
          if password2 == model.login_information.password
            then
              LoginInProgress
                model
                (Cmd.map CreateAccountResponse (createAccountCmd model.login_information))
            else
              LoginInProgress
                ({ model | error="Passwords do not match." })
                Cmd.none
    CreateAccountMode -> LoginInProgress ({ model | maybe_password2=Just ""}) Cmd.none
    CreateAccountResponse response ->
      case response of
        ExpectedResult (AccountCreated token) -> LoginFinished token
        ExpectedResult AccountAlreadyExists ->
          LoginInProgress { model | error="Account already exists!" } Cmd.none
        UnexpectedError error ->
          LoginInProgress { model | error=toString error} Cmd.none
    LoginMode -> LoginInProgress ({ model | maybe_password2=Nothing}) Cmd.none
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
        (
          [ tr []
              [ td [] [text "Username:"]
              , td [] [input [type_ "text", onInput Username] []]
              ]
          , tr []
              [ td [] [text "Password:"]
              , td [] [input [type_ "password", onInput Password] []]
              ]
          ] ++
            case model.maybe_password2 of
              Nothing -> []
              Just _ ->
                [ tr []
                    [ td [] [text "Password (again):"]
                    , td [] [input [type_ "password", onInput Password2] []]
                    ]
                ]
        )
    , case model.maybe_password2 of
        Nothing -> a [ onClick CreateAccountMode ] [ text "Create account" ]
        Just _ -> a [ onClick LoginMode ] [ text "Login" ]
    , div [] [ button [onClick Request] [text "Submit"] ]
    , div [style [("color", "red")]] [text model.error]
    ]
