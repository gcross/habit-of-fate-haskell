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
import Http
import List exposing (foldr)
import Random.Pcg as Random exposing (Generator)
import String
import Task exposing (Task, andThen, mapError, succeed)
import Tuple
import Uuid exposing (Uuid)

import Api exposing (..)


--------------------------------------------------------------------------------
------------------------------------ Tests -------------------------------------
--------------------------------------------------------------------------------


username_generator : Generator String
username_generator =
  Random.list 20 (Random.int (toCode 'a') (toCode 'z') |> Random.map fromCode)
  |> Random.map String.fromList

type TestOutcome = TestPassed | TestFailed String


type Test = Test String (Random.Seed -> Task Http.Error TestOutcome)


test_habit : Habit
test_habit = { name = "name", credits = { success = 1, failure = 0 } }

test_habit_2 : Habit
test_habit_2 = { name = "name", credits = { success = 0, failure = 1 } }


testWithAccount :
  String -> (Token -> Random.Seed -> Task Http.Error TestOutcome) -> Test
testWithAccount name constructTestTask =
  Test name (\seed ->
    let (username, seed2) = Random.step username_generator seed
    in
      createAccount username username
      |> Task.map (\result ->
          case result of
            AccountCreated _ -> TestPassed
            AccountAlreadyExists -> TestFailed "Account already exists."
         )
  )


testWithAccountAndHabitId :
  String -> (Token -> Uuid -> Task Http.Error TestOutcome) -> Test
testWithAccountAndHabitId name constructTestTask =
  testWithAccount name (\token seed ->
    let (habit_id, _) = Random.step Uuid.uuidGenerator seed
    in constructTestTask token habit_id
  )


tests : List Test
tests =
  [ (Test "Creating a new account succeeds." (\seed ->
      let (username, _) = Random.step username_generator seed
      in
        createAccount username username |> Task.map (\result ->
          case result of
            AccountCreated _ -> TestPassed
            AccountAlreadyExists -> TestFailed "Account already existed."
        )
    ))
  , (Test "Logging in to a missing account fails." (\seed ->
      let (username, _) = Random.step username_generator seed
      in
        login username username |> Task.map (\result ->
          case result of
            NoSuchAccount -> TestPassed
            InvalidPassword -> TestFailed "Account existed."
            LoginSuccessful _ -> TestFailed "Login successful."
        )
    ))
  , (Test "Logging into an existing account but with the wrong password." (\seed ->
      let (username, _) = Random.step username_generator seed
      in
        createAccount username username
        |> andThen (\_ -> login username "wrong password")
        |> Task.map (\result ->
            case result of
              InvalidPassword -> TestPassed
              NoSuchAccount -> TestFailed "No such account."
              LoginSuccessful _ -> TestFailed "Login successful."
           )
    ))
  , (Test "Logging into an existing account with the correct password." (\seed ->
      let (username, _) = Random.step username_generator seed
      in
        createAccount username username
        |> andThen (\_ -> login username username)
        |> Task.map (\result ->
            case result of
              LoginSuccessful _ -> TestPassed
              NoSuchAccount -> TestFailed "No such account."
              InvalidPassword -> TestFailed "Invalid password."
           )
    ))
  , (testWithAccountAndHabitId "Putting an new habit results in HabitCreated." (\token habit_id ->
      putHabit token habit_id test_habit |> Task.map (\result ->
        case result of
          HabitCreated -> TestPassed
          HabitReplaced -> TestFailed "Got HabitReplaced"
      )
    ))
  , (testWithAccountAndHabitId "Putting an existing habit results in HabitReplaced." (\token habit_id ->
      putHabit token habit_id test_habit |> andThen (\_ ->
      putHabit token habit_id test_habit |> Task.map (\result ->
        case result of
          HabitCreated -> TestFailed "Got HabitCreated"
          HabitReplaced -> TestPassed
    ))))
  , (testWithAccountAndHabitId "Getting a non-existent habit returns Nothing." (\token habit_id ->
      getHabit token habit_id |> Task.map (\result ->
        case result of
          Nothing -> TestPassed
          Just habit -> TestFailed ("Unexpectedly received: " ++ toString habit)
    )))
  , (testWithAccountAndHabitId "Putting an existing habit results followed by getting it gets the right value." (\token habit_id ->
      putHabit token habit_id test_habit |> andThen (\_ ->
      getHabit token habit_id |> Task.map (\result ->
        case result of
          Nothing -> TestFailed "Could not find the habit."
          Just habit ->
            if habit == test_habit
              then TestPassed
              else TestFailed ("Expected " ++ toString test_habit ++ " but got " ++ toString habit)
    ))))
  , (testWithAccount "Getting all habits when none exist returns the empty list." (\token _ ->
      getHabits token |> Task.map (\habits ->
        if EveryDict.isEmpty habits
          then TestPassed
          else TestFailed ("Unexpectedly received habits: " ++ toString habits)
    )))
  , (testWithAccountAndHabitId "Putting a habit followed by getting all habits returns a singleton list with the habit." (\token habit_id ->
      putHabit token habit_id test_habit |> andThen (\_ ->
      getHabits token |> Task.map (\habits ->
        if habits == EveryDict.singleton habit_id test_habit
          then TestPassed
          else TestFailed ("Expected " ++ toString [test_habit] ++ " but got " ++ toString habits)
    ))))
  , (testWithAccountAndHabitId "Deleting a non-existent habit returns NoHabitToDelete." (\token habit_id ->
      deleteHabit token habit_id |> Task.map (\result ->
        case result of
          NoHabitToDelete -> TestPassed
          HabitDeleted -> TestFailed "Unexpected success."
    )))
  , (testWithAccountAndHabitId  "Deleting an existing habit returns HabitDeleted." (\token habit_id ->
      putHabit token habit_id test_habit |> andThen (\_ ->
      deleteHabit token habit_id |> Task.map (\result ->
        case result of
          HabitDeleted -> TestPassed
          NoHabitToDelete -> TestFailed "The habit was not found when being deleted."
    ))))
  , (testWithAccountAndHabitId "Deleting an existing habit causes it to no longer be found by getHabit." (\token habit_id ->
      putHabit token habit_id test_habit |> andThen (\_ ->
      deleteHabit token habit_id |> andThen (\_ ->
      getHabit token habit_id |> Task.map (\result ->
        case result of
          Nothing -> TestPassed
          Just _ -> TestFailed "The habit was found after being deleted."
    )))))
  , (testWithAccount "Getting credits after account creation gets zeros." (\token _ ->
      getCredits token |> Task.map (\credits ->
        if credits.success == 0 && credits.failure == 0
          then TestPassed
          else TestFailed ("Non-zero credits: " ++ toString credits)
    )))
  , (testWithAccount "Marking habits changes the credits correctly." (\token seed_1 ->
    let (habit_id_1, seed_2) = Random.step Uuid.uuidGenerator seed_1
        (habit_id_2, _) = Random.step Uuid.uuidGenerator seed_2
    in
      putHabit token habit_id_1 test_habit |> andThen (\_ ->
      putHabit token habit_id_2 test_habit_2 |> andThen (\_ ->
      markHabits token
        { successes = [habit_id_1]
        , failures = [habit_id_2]
        } |> andThen (\_ ->
      getCredits token |> Task.map (\credits ->
        let expected_credits =
             { success = test_habit.credits.success + test_habit_2.credits.success
             , failure = test_habit.credits.failure + test_habit_2.credits.failure
             }
        in if credits == expected_credits
          then TestPassed
          else TestFailed ("Expected " ++ toString expected_credits ++
                           " but saw " ++ toString credits
                          )
    ))))))
  ]


--------------------------------------------------------------------------------
----------------------------------- Program ------------------------------------
--------------------------------------------------------------------------------


type alias Model = List (String, TestOutcome)
type Msg = Seed Random.Seed | NewTestResult String TestOutcome


seed_generator : Generator Random.Seed
seed_generator =
  Random.int Random.minInt Random.maxInt |> Random.map Random.initialSeed


init : ( Model, Cmd Msg )
init = ( [], seed_generator |> Random.generate Seed )


startTests : Random.Seed -> Cmd Msg
startTests initial_seed =
  tests
  |> foldr
      (\(Test name makeTask) (seed, rest_cmds) ->
        let (test_seed, next_seed) = Random.step seed_generator seed
        in
          ( next_seed
          , (makeTask seed
             |> Task.attempt (\result ->
                 case result of
                   Ok outcome -> outcome
                   Err error -> TestFailed ("Unexpected error: " ++ toString error)
                )
             |> Cmd.map (NewTestResult name)
            )::rest_cmds
          )
      )
      (initial_seed, [])
  |> Tuple.second
  |> Cmd.batch


update : Msg -> Model -> (Model, Cmd Msg)
update msg old_results =
  case msg of
    Seed seed -> (old_results, startTests seed)
    NewTestResult name outcome -> ((name, outcome)::old_results, Cmd.none)


view : Model -> Html Msg
view results =
  div []
    [ div [ ] [Html.ul [] (
        results
        |> List.map (\(name, outcome) ->
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
