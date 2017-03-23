module Test.Main where

import Prelude
import Control.Monad.Aff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Random
import Data.Char
import Data.Either
import Data.StrMap
import Test.Unit
import Test.Unit.Assert
import Client
import UUID
import Data.String (fromCharArray)
import Data.Unfoldable (replicateA)
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

test_habit = Habit { name: "name", credits: Credits { success: 1.0, failure: 2.0 } }
test_habit_2 = Habit { name: "name", credits: Credits { success: 2.0, failure: 1.0 } }

test_habit_id = "95bef3cf-9031-4f64-8458-884aa6781563"
test_habit_id_2 = "9e801a68-4288-4a23-8779-aa68f94991f9"

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
    void $ createRandomAccount >>= runClient <<< getHabits
  test "fetching a habit after putting it returns the habit" $ do
    session_info ← createRandomAccount
    retrieved_habit ← runClient $ do
      putHabit session_info test_habit_id test_habit
      getHabit session_info test_habit_id
    equal test_habit retrieved_habit
  test "fetching all habits after putting one returns a singleton map" $ do
    session_info ← createRandomAccount
    retrieved_habits ← runClient $ do
      putHabit session_info test_habit_id test_habit
      getHabits session_info
    equal (singleton test_habit_id test_habit) retrieved_habits
  test "fetching all habits after putting then deleting one returns an empty map" $ do
    session_info ← createRandomAccount
    retrieved_habits ← runClient $ do
      putHabit session_info test_habit_id test_habit
      void $ deleteHabit session_info test_habit_id
      getHabits session_info
    equal empty retrieved_habits
