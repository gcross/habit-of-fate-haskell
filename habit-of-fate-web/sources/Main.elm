import Char exposing (fromCode, toCode)
import Formatting exposing ((<>), print, s, string)
import Html exposing
  ( Attribute
  , Html
  , button
  , div
  , input
  , program
  , text
  )
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Random exposing (Generator)
import String


account_info_generator : Generator String
account_info_generator =
  Random.list 20 (Random.int (toCode 'a') (toCode 'z') |> Random.map fromCode)
  |> Random.map String.fromList


type alias ErrorMessage = String
type alias Token = String


makeLoginUrl : String -> String -> String -> String
makeLoginUrl =
  print (string <> s "?username=" <> string <> s "&password=" <> string)


type CreateAccountResult =
    UnexceptedCreateAccountError Http.Error
  | AccountExists
  | AccountCreated Token


createAccount : String -> String -> Cmd CreateAccountResult
createAccount username password =
  Http.send
    (\result ->
       case result of
          Err err ->
            case err of
              Http.BadStatus response ->
                if response.status.code == 409
                  then AccountExists
                  else UnexceptedCreateAccountError err
              _ -> UnexceptedCreateAccountError err
          Ok token -> AccountCreated token
    )
    (
      Http.request
        { method = "POST"
        , headers = []
        , url = makeLoginUrl "api/create" username password
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
    )


type alias Model = List String
type Msg = NewTestResult String


init : ( Model, Cmd Msg )
init = ( [], startTests )


startTests : Cmd Msg
startTests =
  createAccount "username2" "password2"
  |> Cmd.map (toString >> NewTestResult)


update (NewTestResult result) old_results = (result::old_results, Cmd.none)


view content =
  div []
    [ div [ ] (List.map text (List.reverse content))
    ]


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
