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
import Control.Monad.Free
import Data.Char
import Data.Either
import Data.Unfoldable
import Network.HTTP.Affjax
import Test.Unit
import Test.Unit.Assert
import Client
import Control.Monad.Eff.Exception (Error, EXCEPTION, throwException, error)
import Data.String (fromCharArray)
import Test.Unit.Main (runTest)

randomAlphaChar ∷ ∀ ε. Eff (random ∷ RANDOM | ε) Char
randomAlphaChar = do
  char ← fromCharCode <$> randomInt (toCharCode 'A') (toCharCode 'Z')
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

randomAccount ∷ ∀ r. Aff (random ∷ RANDOM | r) LoginInformation
randomAccount = liftEff do
  username ← randomAlphaString
  pure $
    { username: username
    , password: "password"
    , hostname: "localhost"
    , port: 8081
    }

assertFails ∷ ∀ ε α. Aff ε α → Test ε
assertFails action =
  attempt action
  >>=
  assert "should have failed" <<< isLeft

createRandomAccount = randomAccount >>= createAccount

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
  test "fetching habits from new account returns empty array" $ do
    void $ createRandomAccount >>= fetchHabits
