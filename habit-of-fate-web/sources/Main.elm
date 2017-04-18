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
import Task exposing (Task, andThen, mapError, succeed)
import Tuple


username_generator : Generator String
username_generator =
  Random.list 20 (Random.int (toCode 'a') (toCode 'z') |> Random.map fromCode)
  |> Random.map String.fromList


type alias Token = String


makeLoginUrl : String -> String -> String -> String
makeLoginUrl =
  print (string <> s "?username=" <> string <> s "&password=" <> string)


type Error error = Expected error | Unexpected Http.Error

type alias ApiResult error result = Result (Error error) result


-------------------------------- Create Account --------------------------------


type CreateAccountError = AccountAlreadyExists


createAccount : String -> String -> Task Never (ApiResult CreateAccountError Token)
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
            then Expected AccountAlreadyExists
            else Unexpected error
        _ -> Unexpected error
     )))


------------------------------------ Login -------------------------------------


type LoginError = NoSuchAccount | InvalidPassword


login : String -> String -> Task Never (ApiResult LoginError Token)
login username password =
  (
    Http.request
      { method = "POST"
      , headers = []
      , url = makeLoginUrl "api/login" username password
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
          case response.status.code of
            403 -> Expected InvalidPassword
            404 -> Expected NoSuchAccount
            _ -> Unexpected error
        _ -> Unexpected error
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
  , Test "Logging in to a missing account fails." (\seed ->
      let (username, _) = Random.step username_generator seed
      in
        login username username
        |> Task.perform (\result ->
            case result of
              Err (Expected NoSuchAccount) -> TestPassed
              _ -> TestFailed ("Unexpected result: " ++ toString result)
           )
    )
  , Test "Logging into an existing account but with the wrong password." (\seed ->
      let (username, _) = Random.step username_generator seed
      in
        createAccount username username
        |> andThen (\_ -> login username "wrong password")
        |> Task.perform (\result ->
            case result of
              Err (Expected InvalidPassword) -> TestPassed
              _ -> TestFailed ("Unexpected result: " ++ toString result)
           )
    )
  , Test "Logging into an existing account with the correct password." (\seed ->
      let (username, _) = Random.step username_generator seed
      in
        createAccount username username
        |> andThen (\_ -> login username username)
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
    [ div [ ] [Html.ul [] (
        results
        |> List.map (\(TestResult name outcome) ->
            let (color, txt) =
                  case outcome of
                    TestPassed ->
                      ("green", "Test \"" ++ name ++ "\" passed.")
                    TestFailed reason ->
                      ("red", "Test \"" ++ name ++ "\" failed: " ++ reason)
            in Html.li [style [("color", color)]] [text txt]
           )
        |> List.map (\result -> Html.li [] [result])
        |> List.reverse
      )]
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
