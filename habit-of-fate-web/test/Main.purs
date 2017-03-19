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

createRandomAccount =
  randomAccount
  >>=
  createAccount
  >>=
  maybe (throwError <<< error $ "Account was not created.") pure

main = runTest do
  suite "create account" do
    test "succeeds when not present" do
      login_information ← randomAccount
      createAccount login_information >>=
        assert "Account should have been created but was not." <<< isJust
    test "fails when already present" do
      login_information ← randomAccount
      createAccount login_information
      createAccount login_information >>=
        assert "Account should have not been created but was." <<< isNothing
  suite "login" do
    test "fails when not present" do
      login_information ← randomAccount
      login login_information >>=
        assert "Login should have failed but did not." <<< isNothing
    test "succeeds when present" do
      login_information ← randomAccount
      createAccount login_information
      login login_information >>=
        assert "Login should have suceeded but did not." <<< isJust
  test "fetching habits from new account returns empty array" $ do
    void $ createRandomAccount >>= fetchHabits
