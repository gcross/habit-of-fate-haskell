module Page.Account.Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Api exposing (..)


type alias Model =
  { account: { username: String, password: String }
  , error: String
  }


type Msg =
    Username String
  | Password String
  | Request
  | Response (ApiResult LoginResult)


type UpdateResult =
    SwitchToCreate
  | InProgress Model (Cmd Msg)
  | Finished Token


update : Msg -> Model -> UpdateResult
update msg model =
  case msg of
    Username username ->
      InProgress
        { model | account = { model.account | username=username } }
        Cmd.none
    Password password ->
      InProgress
        { model | account = { model.account | password=password } }
        Cmd.none
    Request ->
      case model.maybe_password2 of
        Nothing ->
          InProgress
            model
            (Cmd.map Response (loginCmd model.login_information))
        Just password2 ->
          if password2 == model.login_information.password
            then
              InProgress
                model
                (Cmd.map CreateAccountResponse (createAccountCmd model.login_information))
            else
              InProgress
                ({ model | error="Passwords do not match." })
                Cmd.none
    Response response ->
      case response of
        ExpectedResult (AccountCreated token) -> Finished token
        ExpectedResult AccountAlreadyExists ->
          InProgress { model | error="Account already exists!" } Cmd.none
        UnexpectedError error ->
          InProgress { model | error=toString error} Cmd.none


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
          , tr []
              [ td [] [text "Password (again):"]
              , td [] [input [type_ "password", onInput Password2] []]
              ]
          ]
        )
    , a [ onClick SwitchToLogin ] [ text "Login" ]
    , div [] [ button [onClick Request] [text "Submit"] ]
    , div [style [("color", "red")]] [text model.error]
    ]
