module Api exposing (..)


import Formatting exposing ((<>), print, s, string)
import Http
import Task exposing (Task, succeed)


type alias Token = String


makeLoginUrl : String -> String -> String -> String
makeLoginUrl =
  print (string <> s "?username=" <> string <> s "&password=" <> string)


type Error error = Expected error | Unexpected Http.Error

type alias ApiResult error result = Result (Error error) result


--------------------------------------------------------------------------------
-------------------------------- Create Account --------------------------------
--------------------------------------------------------------------------------


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


--------------------------------------------------------------------------------
------------------------------------ Login -------------------------------------
--------------------------------------------------------------------------------


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
