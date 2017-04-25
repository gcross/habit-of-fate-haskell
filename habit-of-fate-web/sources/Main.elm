import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


import Api exposing (..)


type alias Model =
  { login_information: { username: String, password: String }
  , status: Maybe (Result String Token)
  }
type Msg =
    CreateAccountUsername String
  | CreateAccountPassword String
  | CreateAccountLogin
  | CreateAccountResponse (ApiResult CreateAccountResult)


modifyLoginInformation : (LoginInformation -> LoginInformation) -> Model -> Model
modifyLoginInformation modify model =
  { model | login_information = modify model.login_information }


init : ( Model, Cmd Msg )
init =
  ( { login_information={username="", password=""}
    , status=Nothing
    }
  , Cmd.none
  )


update : Msg -> Model -> (Model, Cmd Msg)
update result model =
  case result of
    CreateAccountUsername username ->
      ( modifyLoginInformation (\login -> { login | username=username }) model
      , Cmd.none
      )
    CreateAccountPassword password ->
      ( modifyLoginInformation (\login -> { login | password=password }) model
      , Cmd.none
      )
    CreateAccountLogin ->
      ( model
      , Cmd.map CreateAccountResponse (createAccountCmd model.login_information)
      )
    CreateAccountResponse response ->
      let new_status =
            case response of
              ExpectedResult (AccountCreated token) -> Just (Ok token)
              ExpectedResult AccountAlreadyExists -> Just (Err "Account already exists!")
              UnexpectedError error -> Just (Err (toString error))
      in ({ model | status=new_status }, Cmd.none)


view : Model -> Html Msg
view model =
  div []
    [ table []
        [ tr []
            [ td [] [text "Username:"]
            , td [] [input [type_ "text", onInput CreateAccountUsername] []]
            ]
        , tr []
            [ td [] [text "Password:"]
            , td [] [input [type_ "password", onInput CreateAccountPassword] []]
            ]
        , tr []
            [ td [] [button [onClick CreateAccountLogin] [text "Create account"]] ]
        ]
    , div []
        (case model.status of
          Nothing -> []
          Just result ->
            let (color, txt) =
                  case result of
                    Err error -> ("red", "Failed: " ++ error)
                    Ok token -> ("green", "Succeeded: " ++ token)
            in [div [style [("color", color)]] [text txt]]
        )
    ]


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
