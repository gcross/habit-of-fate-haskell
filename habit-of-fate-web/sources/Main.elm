import Char exposing (fromCode, toCode)
import EveryDict
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
import List exposing (foldr)
import Random.Pcg as Random exposing (Generator)
import String
import Task exposing (Task, andThen, mapError, succeed)
import Tuple
import Uuid exposing (Uuid)

import Api exposing (..)


--------------------------------------------------------------------------------
------------------------------------- API --------------------------------------
--------------------------------------------------------------------------------


username_generator : Generator String
username_generator =
  Random.list 20 (Random.int (toCode 'a') (toCode 'z') |> Random.map fromCode)
  |> Random.map String.fromList

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


test_habit : Habit
test_habit = { name = "name", credits = { success = 1, failure = 0 } }


testWithAccount : String -> (Token -> Random.Seed -> Task Never TestOutcome) -> Test
testWithAccount name constructTestTask =
  Test name (\seed ->
    let (username, seed2) = Random.step username_generator seed
    in
      createAccount username username |> andThen (\result ->
        case result of
          Err error -> succeed (TestFailed ("Failed creating account: " ++ toString error))
          Ok token -> constructTestTask token seed2
      ) |> Task.perform identity
  )


testWithAccountAndHabitId :
  String -> (Token -> Uuid -> Task Never TestOutcome) -> Test
testWithAccountAndHabitId name constructTestTask =
  testWithAccount name (\token seed ->
    let (habit_id, _) = Random.step Uuid.uuidGenerator seed
    in constructTestTask token habit_id
  )


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
  , testWithAccountAndHabitId "Putting an new habit results in HabitCreated." (\token habit_id ->
      putHabit token habit_id test_habit |> andThen (\result -> succeed (
        case result of
          Ok HabitCreated -> TestPassed
          Ok HabitReplaced -> TestFailed "Got HabitReplaced"
          Err error -> TestFailed ("Failed putting habit: " ++ toString error)
      ))
    )
  , testWithAccountAndHabitId "Putting an existing habit results in HabitReplaced." (\token habit_id ->
      putHabit token habit_id test_habit |> andThen (\_ ->
      putHabit token habit_id test_habit |> andThen (\result -> succeed (
        case result of
          Ok HabitCreated -> TestFailed "Got HabitCreated"
          Ok HabitReplaced -> TestPassed
          Err error -> TestFailed ("Failed putting habit: " ++ toString error)
      ))))
  , testWithAccountAndHabitId "Getting a non-existent habit returns Nothing." (\token habit_id ->
      getHabit token habit_id |> andThen (\result -> succeed (
        case result of
          Ok Nothing -> TestPassed
          Ok (Just habit) -> TestFailed ("Unexpectedly received: " ++ toString habit)
          Err error -> TestFailed ("Failed getting habit: " ++ toString error)
      )))
  , testWithAccountAndHabitId "Putting an existing habit results followed by getting it gets the right value." (\token habit_id ->
      putHabit token habit_id test_habit |> andThen (\_ ->
      getHabit token habit_id |> andThen (\result -> succeed (
        case result of
          Ok Nothing -> TestFailed "Could not find the habit."
          Ok (Just habit) ->
            if habit == test_habit
              then TestPassed
              else TestFailed ("Expected " ++ toString test_habit ++ " but got " ++ toString habit)
          Err error -> TestFailed ("Failed getting habit: " ++ toString error)
      ))))
  , testWithAccount "Getting all habits when none exist returns the empty list." (\token _ ->
      getHabits token |> andThen (\result -> succeed (
        case result of
          Ok habits ->
            if EveryDict.isEmpty habits
              then TestPassed
              else TestFailed ("Unexpectedly received habits: " ++ toString habits)
          Err error -> TestFailed ("Failed getting habits: " ++ toString error)
      )))
  , testWithAccountAndHabitId "Putting a habit followed by getting all habits returns a singleton list with the habit." (\token habit_id ->
      putHabit token habit_id test_habit |> andThen (\_ ->
      getHabits token |> andThen (\result -> succeed (
        case result of
          Ok habits ->
            if habits == EveryDict.singleton habit_id test_habit
              then TestPassed
              else TestFailed ("Expected " ++ toString [test_habit] ++ " but got " ++ toString habits)
          Err error -> TestFailed ("Failed getting habits: " ++ toString error)
      ))))
  , testWithAccountAndHabitId "Deleting a non-existent habit returns NoHabitToDelete." (\token habit_id ->
      deleteHabit token habit_id |> andThen (\result -> succeed (
        case result of
          Ok NoHabitToDelete -> TestPassed
          Ok HabitDeleted -> TestFailed "Unexpected success."
          Err error -> TestFailed ("Unexpected failure: " ++ toString error)
      )))
  , testWithAccountAndHabitId  "Deleting an existing habit returns HabitDeleted." (\token habit_id ->
      putHabit token habit_id test_habit |> andThen (\_ ->
      deleteHabit token habit_id |> andThen (\result -> succeed (
        case result of
          Ok HabitDeleted -> TestPassed
          Ok NoHabitToDelete -> TestFailed "The habit was not found when being deleted."
          Err error -> TestFailed ("Unexpected failure: " ++ toString error)
      ))))
  , testWithAccountAndHabitId "Deleting an existing habit causes it to no longer be found by getHabit." (\token habit_id ->
      putHabit token habit_id test_habit |> andThen (\_ ->
      deleteHabit token habit_id |> andThen (\_ ->
      getHabit token habit_id |> andThen (\result -> succeed (
        case result of
          Ok Nothing -> TestPassed
          Ok (Just _) -> TestFailed ("The habit was found after being deleted.")
          Err error -> TestFailed ("Unexpected failure: " ++ toString error)
      )))))
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
