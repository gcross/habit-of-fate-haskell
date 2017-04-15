module Main where

import Prelude
import Control.Monad.Aff
import Control.Monad.Aff.Class
import Control.Monad.Aff.Console
import Control.Monad.Eff
import Control.Monad.Error.Class
import Data.Maybe (Maybe(..))
import Data.Time.Duration
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax

import Client
import Debug.Trace

type State = Boolean

data Query a
  = Toggle a
  | IsOn (Boolean -> a)

data Message = Toggled Boolean

myButton :: ∀ aff. H.Component HH.HTML Query Unit Message (Aff (console ∷ CONSOLE | aff))
myButton =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = false

  render :: State -> H.ComponentHTML Query
  render state =
    let
      label = if state then "On" else "Off"
    in
      trace "TRACE" $ \_ →
      HH.button
        [ HP.title label
        , HE.onClick (HE.input_ Toggle)
        ]
        [ HH.text label ]

  eval :: ∀ aff. Query ~> H.ComponentDSL State Query Message (Aff (console ∷ CONSOLE | aff))
  eval x = do
    liftAff (liftAff $ log "EVAL")
    case x of
      Toggle next -> do
        state <- H.get
        let nextState = not state
        H.put nextState
        H.raise $ Toggled nextState
        pure next
      IsOn reply -> do
        state <- H.get
        pure (reply state)

main ∷ Eff (HA.HalogenEffects (ajax ∷ AJAX, console ∷ CONSOLE)) Unit
main = HA.runHalogenAff do
  log "HELLO!"
  {-(runClient do
    (do session ← login { username: "username", password: "password", hostname: "localhost", port: 8081 }
        liftAff $ log session.token
     )
     `catchError`
     (\e → liftAff $ log (show e))
   )-}
  log "GOODBYE!"
  delay $ Milliseconds 1
  body <- HA.awaitBody
  log "SEEYA!"
  runUI myButton unit body
