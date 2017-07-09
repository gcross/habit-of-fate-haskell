import Array exposing (Array)
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


type alias Test = Random.Seed -> Task Http.Error TestOutcome


test_habit : Habit
test_habit =
  {
    name = "name",
    importance = VeryHigh,
    difficulty = Medium
  }

test_habit_2 : Habit
test_habit_2 =
  {
    name = "name",
    importance = Low,
    difficulty = Low
  }


withAccount : (Token -> Random.Seed -> Task Http.Error TestOutcome) -> Test
withAccount constructTestTask seed =
  let (username, next_seed) = Random.step username_generator seed
  in
    createAccountWherePasswordIsUsername username
    |> andThen (\result ->
        case result of
          AccountCreated token -> constructTestTask token next_seed
          AccountAlreadyExists -> succeed (TestFailed "Account already exists.")
        )


withAccountAndHabitId : (Token -> Uuid -> Task Http.Error TestOutcome) -> Test
withAccountAndHabitId constructTestTask =
  withAccount (\token seed ->
    let (habit_id, _) = Random.step Uuid.uuidGenerator seed
    in constructTestTask token habit_id
  )


createAccountWherePasswordIsUsername : String -> Task Http.Error CreateAccountResult
createAccountWherePasswordIsUsername username =
  createAccountTask { username=username, password=username }


loginWherePasswordIsUsername : String -> Task Http.Error LoginResult
loginWherePasswordIsUsername username =
  loginTask { username=username, password=username }


tests : List (String, Test)
tests =
  [ ("Creating a new account succeeds.", \seed ->
      let (username, _) = Random.step username_generator seed
      in
        createAccountWherePasswordIsUsername username |> Task.map (\result ->
          case result of
            AccountCreated _ -> TestPassed
            AccountAlreadyExists -> TestFailed "Account already existed."
        )
    )
  , ("Logging in to a missing account fails.", \seed ->
      let (username, _) = Random.step username_generator seed
      in
        loginWherePasswordIsUsername username |> Task.map (\result ->
          case result of
            NoSuchAccount -> TestPassed
            InvalidPassword -> TestFailed "Account existed."
            LoginSuccessful _ -> TestFailed "LoginTask successful."
        )
    )
  , ("Logging into an existing account but with the wrong password.", \seed ->
      let (username, _) = Random.step username_generator seed
      in
        createAccountWherePasswordIsUsername username
        |> andThen (\_ -> loginTask { username=username, password="wrong password" })
        |> Task.map (\result ->
            case result of
              InvalidPassword -> TestPassed
              NoSuchAccount -> TestFailed "No such account."
              LoginSuccessful _ -> TestFailed "LoginTask successful."
           )
    )
  , ("Logging into an existing account with the correct password.", \seed ->
      let (username, _) = Random.step username_generator seed
      in
        createAccountWherePasswordIsUsername username
        |> andThen (\_ -> loginWherePasswordIsUsername username)
        |> Task.map (\result ->
            case result of
              LoginSuccessful _ -> TestPassed
              NoSuchAccount -> TestFailed "No such account."
              InvalidPassword -> TestFailed "Invalid password."
           )
    )
  , ("Putting an new habit results in HabitCreated.", withAccountAndHabitId (\token habit_id ->
      putHabitTask token habit_id test_habit |> Task.map (\result ->
        case result of
          HabitCreated -> TestPassed
          HabitReplaced -> TestFailed "Got HabitReplaced"
      )
    ))
  , ("Putting an existing habit results in HabitReplaced.", withAccountAndHabitId (\token habit_id ->
      putHabitTask token habit_id test_habit |> andThen (\_ ->
      putHabitTask token habit_id test_habit |> Task.map (\result ->
        case result of
          HabitCreated -> TestFailed "Got HabitCreated"
          HabitReplaced -> TestPassed
    ))))
  , ("Getting a non-existent habit returns Nothing.", withAccountAndHabitId (\token habit_id ->
      getHabitTask token habit_id |> Task.map (\result ->
        case result of
          Nothing -> TestPassed
          Just habit -> TestFailed ("Unexpectedly received: " ++ toString habit)
    )))
  , ("Putting an existing habit results followed by getting it gets the right value.", withAccountAndHabitId (\token habit_id ->
      putHabitTask token habit_id test_habit |> andThen (\_ ->
      getHabitTask token habit_id |> Task.map (\result ->
        case result of
          Nothing -> TestFailed "Could not find the habit."
          Just habit ->
            if habit == test_habit
              then TestPassed
              else TestFailed ("Expected " ++ toString test_habit ++ " but got " ++ toString habit)
    ))))
  , ("Getting all habits when none exist returns the empty list.", withAccount (\token _ ->
      getHabitsTask token |> Task.map (\habits ->
        if EveryDict.isEmpty habits
          then TestPassed
          else TestFailed ("Unexpectedly received habits: " ++ toString habits)
    )))
  , ("Putting a habit followed by getting all habits returns a singleton list with the habit.", withAccountAndHabitId (\token habit_id ->
      putHabitTask token habit_id test_habit |> andThen (\_ ->
      getHabitsTask token |> Task.map (\habits ->
        if habits == EveryDict.singleton habit_id test_habit
          then TestPassed
          else TestFailed ("Expected " ++ toString [test_habit] ++ " but got " ++ toString habits)
    ))))
  , ("Deleting a non-existent habit returns NoHabitToDelete.", withAccountAndHabitId (\token habit_id ->
      deleteHabitTask token habit_id |> Task.map (\result ->
        case result of
          NoHabitToDelete -> TestPassed
          HabitDeleted -> TestFailed "Unexpected success."
    )))
  , ("Deleting an existing habit returns HabitDeleted.", withAccountAndHabitId (\token habit_id ->
      putHabitTask token habit_id test_habit |> andThen (\_ ->
      deleteHabitTask token habit_id |> Task.map (\result ->
        case result of
          HabitDeleted -> TestPassed
          NoHabitToDelete -> TestFailed "The habit was not found when being deleted."
    ))))
  , ("Deleting an existing habit causes it to no longer be found by getHabit.", withAccountAndHabitId (\token habit_id ->
      putHabitTask token habit_id test_habit |> andThen (\_ ->
      deleteHabitTask token habit_id |> andThen (\_ ->
      getHabitTask token habit_id |> Task.map (\result ->
        case result of
          Nothing -> TestPassed
          Just _ -> TestFailed "The habit was found after being deleted."
    )))))
  , ("Getting credits after account creation gets zeros.", withAccount (\token _ ->
      getCreditsTask token |> Task.map (\credits ->
        if credits.success == 0 && credits.failure == 0
          then TestPassed
          else TestFailed ("Non-zero credits: " ++ toString credits)
    )))
  , ("Marking habits changes the credits correctly.", withAccount (\token seed_1 ->
    let (habit_id_1, seed_2) = Random.step Uuid.uuidGenerator seed_1
        (habit_id_2, _) = Random.step Uuid.uuidGenerator seed_2
    in
      putHabitTask token habit_id_1 test_habit |> andThen (\_ ->
      putHabitTask token habit_id_2 test_habit_2 |> andThen (\_ ->
      markHabitsTask token
        { successes = [habit_id_1]
        , failures = [habit_id_2]
        } |> andThen (\_ ->
      getCreditsTask token |> Task.map (\credits ->
        let expected_credits =
             { success = scaleFactor test_habit.difficulty
             , failure = scaleFactor test_habit_2.importance
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


type alias Model = Array (Maybe TestOutcome)
type Msg = Seed Random.Seed | NewTestOutcome Int TestOutcome


seed_generator : Generator Random.Seed
seed_generator =
  Random.int Random.minInt Random.maxInt |> Random.map Random.initialSeed


init : ( Model, Cmd Msg )
init = ( Array.repeat (List.length tests) Nothing, seed_generator |> Random.generate Seed )


startTests : Random.Seed -> Cmd Msg
startTests initial_seed =
  tests
  |> foldr
      (\(name, makeTask) (index, seed, rest_cmds) ->
        let (test_seed, next_seed) = Random.step seed_generator seed
        in
          ( index + 1
          , next_seed
          , (makeTask seed
             |> Task.attempt (\result ->
                 case result of
                   Ok outcome -> outcome
                   Err error -> TestFailed ("Unexpected error: " ++ toString error)
                )
             |> Cmd.map (NewTestOutcome index)
            )::rest_cmds
          )
      )
      (0, initial_seed, [])
  |> (\(_, _, cmds) -> cmds)
  |> Cmd.batch


update : Msg -> Model -> (Model, Cmd Msg)
update msg outcomes =
  case msg of
    Seed seed -> (outcomes, startTests seed)
    NewTestOutcome index outcome ->
      ( Array.set index (Just outcome) outcomes
      , Cmd.none
      )


view : Model -> Html Msg
view outcomes =
  div []
    [ div [ ] [Html.ul [] (
        List.map2 (,)
          (tests |> List.map Tuple.first)
          (outcomes |> Array.toList)
        |> List.map (\(name, maybe_outcome) ->
            let (color, txt) =
                  case maybe_outcome of
                    Nothing ->
                      ("black", "Test \"" ++ name ++ "\" is still running....")
                    Just TestPassed ->
                      ("green", "Test \"" ++ name ++ "\" passed.")
                    Just (TestFailed reason) ->
                      ("red", "Test \"" ++ name ++ "\" failed: " ++ reason)
            in Html.li [style [("color", color)]] [text txt]
           )
        |> List.map (\result -> Html.li [] [result])
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
