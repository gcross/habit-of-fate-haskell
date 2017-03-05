module Test.Main where

import Prelude

import Control.Alternative
import Control.Applicative
import Control.Monad
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception (Error, EXCEPTION, throwException, error)
import Control.Monad.Eff.Random
import Data.Char
import Data.Either
import Data.String (fromCharArray)
import Data.Unfoldable
import Test.Unit.Assert (assert)
import Test.Unit (failure, success, suite, test, timeout)
import Test.Unit.Main (runTest)

import Client

randomAlphaChar ∷ ∀ ε. Eff (random ∷ RANDOM | ε) Char
randomAlphaChar = do
  char ← fromCharCode <$> randomInt (toCharCode 'A') (toCharCode 'B')
  make_lowercase ← randomBool
  pure $
    if make_lowercase
      then toLower char
      else char

randomAlphaString ∷ ∀ ε. Eff (random ∷ RANDOM | ε) String
randomAlphaString =
  fromCharArray
  <$>
  replicateA 10 randomAlphaChar

randomAccount ∷ ∀ ε. Aff (random ∷ RANDOM | ε) LoginInformation
randomAccount = liftEff do
  username ← randomAlphaString
  pure $
    { username: username
    , password: "password"
    , hostname: "localhost"
    , port: 8081
    }

assertFails ∷ ∀ ε α. Aff ε α → Aff ε Unit
assertFails action =
  attempt action
  >>=
  assert "should have failed" <<< isLeft

main = runTest do
  suite "create account" do
    test "not present" do
      login_information ← randomAccount
      createAccount login_information
      pure unit
    test "already present" do
      login_information ← randomAccount
      createAccount login_information
      assertFails $ createAccount login_information
      pure unit
  suite "login" do
    test "not present" do
      login_information ← randomAccount
      assertFails $ login login_information
      pure unit
    test "already present" do
      login_information ← randomAccount
      createAccount login_information
      login login_information
      pure unit
