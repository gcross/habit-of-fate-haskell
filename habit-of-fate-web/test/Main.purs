module Test.Main where

import Prelude
import Control.Alternative
import Control.Applicative
import Control.Monad
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff.Random
import Control.Monad.Error.Class
import Control.Monad.Free
import Data.Char
import Data.Either
import Data.Maybe
import Data.Unfoldable hiding (fromMaybe)
import Network.HTTP.Affjax
import Test.Unit
import Test.Unit.Assert
import Client
import Control.Monad.Eff.Exception (Error, EXCEPTION, throwException, error)
import Data.String (fromCharArray)
import Test.Unit.Main (runTest)

randomAlphaChar = do
  char ← fromCharCode <$> randomInt (toCharCode 'A') (toCharCode 'Z')
  make_lowercase ← randomBool
  pure $
    if make_lowercase
      then toLower char
      else char

randomAlphaString =
  fromCharArray
  <$>
  replicateA 10 randomAlphaChar

randomAccount = liftEff do
  username ← randomAlphaString
  pure $
    { username: username
    , password: "password"
    , hostname: "localhost"
    , port: 8081
    }

assertFails action =
  attempt action
  >>=
  assert "should have failed" <<< isLeft

createRandomAccount = randomAccount >>= (runClient <<< createAccount)

main = runTest do
  suite "create account" do
    test "succeeds when not present" do
      login_information ← randomAccount
      void <<< runClient $ createAccount login_information
    test "fails when already present" do
      login_information ← randomAccount
      runClient $ createAccount login_information
      expectFailure "Second createAccount should have failed." $
        void <<< runClient $ createAccount login_information
  suite "login" do
    test "fails when not present" do
      login_information ← randomAccount
      expectFailure "Login before createAccount should have failed." $
        void <<< runClient $ login login_information
    test "succeeds when present" do
      login_information ← randomAccount
      void <<< runClient $ do
        createAccount login_information
        login login_information
  test "fetching habits from new account returns empty array" $ do
    void $ createRandomAccount >>= runClient <<< fetchHabits
