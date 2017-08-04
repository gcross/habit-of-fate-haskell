module Page.Habit exposing (..)

view model =
  div []
    [ table []
      [ tr []
          [ td [] [text "Name:"]
          , from_
              [VeryLow, Low, Medium, High, VeryHigh]
              Difficulty
              toString
              (\scale ->
                 case scale of
                   VeryLow -> "Very Easy"
                   Low -> "Easy"
                   Medium -> "Medium"
                   High -> "Difficult"
                   VeryHigh -> "Very Difficult"
              )
          ]
      ]
    , case model.maybe_password2 of
        Nothing -> a [ onClick CreateAccountMode ] [ text "Create account" ]
        Just _ -> a [ onClick LoginMode ] [ text "Login" ]
    , div [] [ button [onClick Request] [text "Submit"] ]
    , div [style [("color", "red")]] [text model.error]
    ]
