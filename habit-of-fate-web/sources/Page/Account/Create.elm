module Page.Account.Create exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Api exposing (..)


type alias GenericModel r =
  { account: Account r
  , error: String
  }


type alias Model = GenericModel { password2 : String}


type Msg result =
    Username String
  | Password String
  | Password2 String
  | SwitchToLogin
  | Submit
  | Response (ApiResult result)


type GenericUpdateResult r msg =
    Switch
  | InProgress (GenericModel r) (Cmd msg)
  | Finished Token


update : Msg -> Model -> UpdateResult
update msg model =
  let account = model.account in
  case msg of
    Username username ->
      InProgress
        { model | account={ account | username=username } }
        Cmd.none
    Password password ->
      InProgress
        { model | account={ account | password=password } }
        Cmd.none
    Password2 password2 ->
      InProgress
        { model | account={ account | password2=password2 } }
        Cmd.none
    SwitchToLogin -> Switch
    Submit ->
      if account.password == account.password2
        then
          InProgress
            model
            (Cmd.map Response (createAccountCmd model.login_information))
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
