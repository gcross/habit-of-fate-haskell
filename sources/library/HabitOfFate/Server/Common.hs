{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2017 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Common where

import HabitOfFate.Prelude

import Control.Concurrent.STM.TVar (TVar, readTVarIO)
import Control.Concurrent.STM.TMVar (TMVar)
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Text.Lazy as Lazy
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID, fromText)
import Network.HTTP.Types.Status (badRequest400)
import Network.Wai (rawPathInfo, rawQueryString, requestMethod)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html, (!), toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ActionM, Parsable(..))
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Logging

instance Parsable UUID where
  parseParam = view strict >>> fromText >>> maybe (Left "badly formed UUID") Right

newtype Cookie = Cookie Text deriving (Eq,FromJSON,Ord,Parsable,Read,Show,ToJSON)

data Environment = Environment
  { accounts_tvar ∷ TVar (Map Username (TVar Account))
  , accounts_changed_flag ∷ TVar Bool
  , cookies_tvar ∷ TVar (Map Cookie (UTCTime, Username))
  , expirations_tvar ∷ TVar (Set (UTCTime, Cookie))
  , test_mode ∷ Bool
  }

readTVarMonadIO ∷ MonadIO m ⇒ TVar α → m α
readTVarMonadIO = readTVarIO >>> liftIO

paramGuardingAgainstMissing ∷ Parsable α ⇒ Lazy.Text → ActionM α
paramGuardingAgainstMissing name =
  Scotty.param name
  `Scotty.rescue`
  (\_ → do
    Scotty.status badRequest400
    Scotty.text $ name ⊕ " was not given"
    Scotty.finish
   )

renderPage ∷ Text → [Text] → Html → Lazy.Text
renderPage title stylesheets content =
  renderHtml $
    H.docTypeHtml $
      (H.head <<< mconcat $
        [ H.title $ toHtml title
        , H.link ! A.href "https://fonts.googleapis.com/css?family=Gloria+Hallelujah" ! A.rel "stylesheet"
        , mconcat
          [ H.link
              ! A.rel "stylesheet"
              ! A.type_ "text/css"
              ! A.href (H.toValue $ mconcat ["/css/", stylesheet, ".css"])
          | stylesheet ← "normalize":"common":stylesheets
          ]
        ]
      )
      ⊕
      (H.body content)

generateTopHTML ∷ Html → Html
generateTopHTML content = H.div ! A.class_ "top" $ do
  H.div ! A.class_ "logo" $ H.img ! A.src "/images/logo.svgz" ! A.width "100%"
  H.div ! A.class_ "left" $ H.img ! A.src "/images/treasure-chest.svgz" ! A.width "100%"
  H.div ! A.class_ "right" $ H.img ! A.src "/images/grave.svgz" ! A.width "100%"
  H.div ! A.class_ "content" $ content

renderTopOnlyPage ∷ Text → [Text] → Html → Lazy.Text
renderTopOnlyPage title stylesheets = generateTopHTML >>> renderPage title stylesheets
