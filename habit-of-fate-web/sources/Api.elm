module Api exposing (..)


import EveryDict exposing (EveryDict)
import Formatting exposing ((<>), print, s, string)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List exposing (foldr)
import Task exposing (Task, fail, succeed)
import Uuid exposing (Uuid)


--------------------------------------------------------------------------------
-------------------------------- Login-related ---------------------------------
--------------------------------------------------------------------------------


type alias Token = String


makeLoginUrl : String -> String -> String -> String
makeLoginUrl =
  print (string <> s "?username=" <> string <> s "&password=" <> string)


-------------------------------- Create Account --------------------------------


type CreateAccountResult = AccountAlreadyExists | AccountCreated Token


createAccount : String -> String -> Task Http.Error CreateAccountResult
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
  |> Task.map AccountCreated
  |> Task.onError (\error ->
      case error of
        Http.BadStatus response ->
          if response.status.code == 409
            then succeed AccountAlreadyExists
            else fail error
        _ -> fail error
     )


------------------------------------ Login -------------------------------------


type LoginResult = NoSuchAccount | InvalidPassword | LoginSuccessful Token


login : String -> String -> Task Http.Error LoginResult
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
  |> Task.map LoginSuccessful
  |> Task.onError (\error ->
      case error of
        Http.BadStatus response ->
          case response.status.code of
            403 -> succeed InvalidPassword
            404 -> succeed NoSuchAccount
            _ -> fail error
        _ -> fail error
     )


--------------------------------------------------------------------------------
-------------------------------- Habit-related ---------------------------------
--------------------------------------------------------------------------------


type alias Credits = { success: Float, failure: Float }
type alias Habit = { name: String, credits: Credits }


makeHabitUrl : Uuid -> String
makeHabitUrl = Uuid.toString >> print (s "/api/habits/" <> string)


uuid_decoder : Decoder Uuid
uuid_decoder =
  Decode.string
  |> Decode.andThen (\s ->
      case Uuid.fromString s of
        Nothing -> Decode.fail "invalid UUID"
        Just uuid -> Decode.succeed uuid
     )


credit_decoder : Decoder Credits
credit_decoder =
  Decode.map2
    (\success failure -> { success = success, failure = failure })
    (Decode.field "success" Decode.float)
    (Decode.field "failure" Decode.float)


habit_decoder : Decoder Habit
habit_decoder =
  Decode.map2
    (\name credits -> { name = name, credits = credits })
    (Decode.field "name" Decode.string)
    (Decode.field "credits" credit_decoder)


habits_decoder : Decoder (EveryDict Uuid Habit)
habits_decoder =
  Decode.keyValuePairs habit_decoder
  |> Decode.andThen (
      foldr
        (\(uuid_string, habit) decoded ->
          case Uuid.fromString uuid_string of
            Nothing -> Decode.fail ("Invalid UUID: " ++ uuid_string)
            Just uuid -> Decode.map (\pairs -> (uuid, habit)::pairs) decoded
        )
        (Decode.succeed [])
     )
  |> Decode.map EveryDict.fromList



encodeCredits : Credits -> Value
encodeCredits credits =
  Encode.object
  [ ("success", Encode.float credits.success)
  , ("failure", Encode.float credits.failure)
  ]


encodeHabit : Habit -> Value
encodeHabit habit =
  Encode.object
  [ ("name", Encode.string habit.name)
  , ("credits", encodeCredits habit.credits)
  ]


---------------------------------- Put Habit -----------------------------------


type PutHabitResult = HabitCreated | HabitReplaced


putHabit : Token -> Uuid -> Habit -> Task Http.Error PutHabitResult
putHabit token uuid habit =
  (
    Http.request
      { method = "PUT"
      , headers = [Http.header "Authorization" ("Bearer " ++ token)]
      , url = makeHabitUrl uuid
      , body = Http.jsonBody (encodeHabit habit)
      , expect = Http.expectStringResponse
          (\response ->
             case response.status.code of
               201 -> Ok HabitCreated
               204 -> Ok HabitReplaced
               _ -> Err ("Unexpected response: " ++ toString response.status)
          )
      , timeout = Nothing
      , withCredentials = False
      }
  )
  |> Http.toTask


---------------------------------- Get Habit -----------------------------------


getHabit : Token -> Uuid -> Task Http.Error (Maybe Habit)
getHabit token uuid =
  (
    Http.request
      { method = "GET"
      , headers = [Http.header "Authorization" ("Bearer " ++ token)]
      , url = makeHabitUrl uuid
      , body = Http.emptyBody
      , expect = Http.expectStringResponse
          (\response ->
             case response.status.code of
               200 ->
                 response.body
                 |> Decode.decodeString habit_decoder
                 |> Result.map Just
               _ -> Err ("Unexpected response: " ++ toString response.status)
          )
      , timeout = Nothing
      , withCredentials = False
      }
  )
  |> Http.toTask
  |> Task.onError (\error ->
      case error of
        Http.BadStatus response ->
          if response.status.code == 404
            then succeed Nothing
            else fail error
        _ -> fail error
     )


---------------------------------- Get Habits ----------------------------------


getHabits : Token -> Task Http.Error (EveryDict Uuid Habit)
getHabits token =
  (
    Http.request
      { method = "GET"
      , headers = [Http.header "Authorization" ("Bearer " ++ token)]
      , url = "/api/habits"
      , body = Http.emptyBody
      , expect = Http.expectJson habits_decoder
      , timeout = Nothing
      , withCredentials = False
      }
  )
  |> Http.toTask


--------------------------------- Delete Habit ---------------------------------


type DeleteHabitResult = HabitDeleted | NoHabitToDelete


deleteHabit : Token -> Uuid -> Task Http.Error DeleteHabitResult
deleteHabit token uuid =
  (
    Http.request
      { method = "DELETE"
      , headers = [Http.header "Authorization" ("Bearer " ++ token)]
      , url = makeHabitUrl uuid
      , body = Http.emptyBody
      , expect = Http.expectStringResponse
          (\response ->
             case response.status.code of
               204 -> Ok HabitDeleted
               _ -> Err ("Unexpected response: " ++ toString response.status)
          )
      , timeout = Nothing
      , withCredentials = False
      }
  )
  |> Http.toTask
  |> Task.onError (\error ->
      case error of
        Http.BadStatus response ->
          if response.status.code == 404
            then succeed NoHabitToDelete
            else fail error
        _ -> fail error
      )
