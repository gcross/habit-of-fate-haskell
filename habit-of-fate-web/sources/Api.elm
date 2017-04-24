module Api exposing (..)


import EveryDict exposing (EveryDict)
import Formatting exposing ((<>), print, s, string)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List exposing (foldr)
import Task exposing (Task, fail, succeed)
import Uuid exposing (Uuid)


handleErrorStatusCode : Int -> α -> Task Http.Error α -> Task Http.Error α
handleErrorStatusCode code value =
  Task.onError (\error ->
    case error of
      Http.BadStatus response ->
        if response.status.code == code
          then succeed value
          else fail error
      _ -> fail error
  )


--------------------------------------------------------------------------------
-------------------------------- Login-related ---------------------------------
--------------------------------------------------------------------------------


type alias Token = String


type alias LoginInformation = { username: String, password: String }


encodeLoginInformation : LoginInformation -> Value
encodeLoginInformation login_information =
  Encode.object
  [ ("username", Encode.string login_information.username)
  , ("password", Encode.string login_information.password)
  ]


-------------------------------- Create Account --------------------------------


type CreateAccountResult = AccountAlreadyExists | AccountCreated Token


createAccount : LoginInformation -> Task Http.Error CreateAccountResult
createAccount login_information =
  (
    Http.request
      { method = "POST"
      , headers = []
      , url = "api/create"
      , body = Http.jsonBody (encodeLoginInformation login_information)
      , expect = Http.expectString
      , timeout = Nothing
      , withCredentials = False
      }
  )
  |> Http.toTask
  |> Task.map AccountCreated
  |> handleErrorStatusCode 409 AccountAlreadyExists


------------------------------------ Login -------------------------------------


type LoginResult = NoSuchAccount | InvalidPassword | LoginSuccessful Token


login : LoginInformation -> Task Http.Error LoginResult
login login_information =
  (
    Http.request
      { method = "POST"
      , headers = []
      , url = "api/login"
      , body = Http.jsonBody (encodeLoginInformation login_information)
      , expect = Http.expectString
      , timeout = Nothing
      , withCredentials = False
      }
  )
  |> Http.toTask
  |> Task.map LoginSuccessful
  |> handleErrorStatusCode 403 InvalidPassword
  |> handleErrorStatusCode 404 NoSuchAccount


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
  |> handleErrorStatusCode 404 Nothing


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
  |> handleErrorStatusCode 404 NoHabitToDelete


--------------------------------------------------------------------------------
--------------------------------- Game-related ---------------------------------
--------------------------------------------------------------------------------


--------------------------------- Get Credits ----------------------------------


getCredits : Token -> Task Http.Error Credits
getCredits token =
  (
    Http.request
      { method = "GET"
      , headers = [Http.header "Authorization" ("Bearer " ++ token)]
      , url = "/api/credits"
      , body = Http.emptyBody
      , expect = Http.expectJson credit_decoder
      , timeout = Nothing
      , withCredentials = False
      }
  )
  |> Http.toTask


--------------------------------- Mark Habits ----------------------------------

type alias Marks = { successes: List Uuid, failures: List Uuid }


encodeUuid : Uuid -> Value
encodeUuid = Uuid.toString >> Encode.string


encodeMarks : Marks -> Value
encodeMarks marks =
  Encode.object
  [ ("successes", marks.successes |> List.map encodeUuid |> Encode.list)
  , ("failures", marks.failures |> List.map encodeUuid |> Encode.list)
  ]


markHabits : Token -> Marks -> Task Http.Error Credits
markHabits token marks =
  (
    Http.request
      { method = "POST"
      , headers = [Http.header "Authorization" ("Bearer " ++ token)]
      , url = "/api/mark"
      , body = Http.jsonBody (encodeMarks marks)
      , expect = Http.expectJson credit_decoder
      , timeout = Nothing
      , withCredentials = False
      }
  )
  |> Http.toTask
