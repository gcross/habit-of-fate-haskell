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
import Http
import List exposing (foldr)
import Random exposing (Generator)
import String
import Task exposing (Task, mapError, succeed)
import Tuple


username_generator : Generator String
username_generator =
  Random.list 20 (Random.int (toCode 'a') (toCode 'z') |> Random.map fromCode)
  |> Random.map String.fromList


type alias Token = String


makeLoginUrl : String -> String -> String -> String
makeLoginUrl =
  print (string <> s "?username=" <> string <> s "&password=" <> string)


type CreateAccountError =
    AccountAlreadyExists
  | UnexpectedCreateAccountError Http.Error


createAccount : String -> String -> Task Never (Result CreateAccountError Token)
createAccount username password =
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
  |> Http.toTask
  |> Task.map Ok
  |> Task.onError (\error -> succeed (Err (
      case error of
        Http.BadStatus response ->
          if response.status.code == 409
            then AccountAlreadyExists
            else UnexpectedCreateAccountError error
        _ -> UnexpectedCreateAccountError error
     )))

type TestOutcome = TestPassed | TestFailed String
type TestResult = TestResult String TestOutcome

type alias Model = List TestResult
type Msg = Seed Random.Seed | NewTestResult TestResult


seed_generator =
  Random.int Random.minInt Random.maxInt |> Random.map Random.initialSeed


init : ( Model, Cmd Msg )
init = ( [], seed_generator |> Random.generate Seed )


type Test = Test String (Random.Seed -> Cmd TestOutcome)


expectSuccess : Task Never (Result err ok) -> Cmd TestOutcome
expectSuccess =
  Task.perform (
    \result ->
      case result of
        Ok _ -> TestPassed
        Err error -> TestFailed (toString error)
  )


swallowResult : Task x a -> Task x ()
swallowResult = Task.map (\_ -> ())


tests : List Test
tests =
  [ Test "Creating a new account succeeds." (\seed ->
      let (username, _) = Random.step username_generator seed
      in
        createAccount username username
        |> expectSuccess
    )
  ]


startTests : Random.Seed -> Cmd Msg
startTests initial_seed =
  tests
  |> foldr
      (\(Test name makeCmd) (seed, rest_cmds) ->
        let (test_seed, next_seed) = Random.step seed_generator seed
        in
          ( next_seed
          , (makeCmd seed |> Cmd.map (TestResult name >> NewTestResult))::rest_cmds
          )
      )
      (initial_seed, [])
  |> Tuple.second
  |> Cmd.batch


update msg old_results =
  case msg of
    Seed seed -> (old_results, startTests seed)
    NewTestResult result -> (result::old_results, Cmd.none)


view : Model -> Html Msg
view results =
  div []
    [ div [ ] (
        results
        |> List.map (\(TestResult name outcome) ->
            case outcome of
              TestPassed -> text ("Test \"" ++ name ++ "\" passed.")
              TestFailed reason -> text ("Test \"" ++ name ++ "\" failed: " ++ reason)
           )
        |> List.reverse
      )
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
