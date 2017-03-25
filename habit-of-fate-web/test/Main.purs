module Test.Main where

import Prelude
import Control.Monad.Aff
import Control.Monad.Aff.Class
import Control.Monad.Eff.Class
import Control.Monad.Eff.Random
import Data.Char
import Data.Either
import Data.Newtype
import Data.StrMap
import Data.String (fromCharArray)
import Data.Unfoldable (replicateA)
import Test.Unit
import Test.Unit.Assert
import Test.Unit.Main (runTest)

import Client
import UUID

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
  suite "create account..." do
    test "...succeeds when the account is not already present" do
      login_information ← randomAccount
      void <<< runClient $ createAccount login_information
    test "...fails when the account is already present" do
      login_information ← randomAccount
      runClient $ createAccount login_information
      expectFailure "Second createAccount should have failed." $
        void <<< runClient $ createAccount login_information
  suite "login..." do
    test "...fails when the account is not present" do
      login_information ← randomAccount
      expectFailure "Login before createAccount should have failed." $
        void <<< runClient $ login login_information
    test "...fails when the account is present but the password is wrong" do
      login_information ← randomAccount
      expectFailure "Login with the wrong password should have failed" $
        runClient $ do
            createAccount login_information
            void <<< login $ login_information { password = "the wrong password" }
    test "...succeeds when the account is present" do
      login_information ← randomAccount
      void <<< runClient $ do
        createAccount login_information
        login login_information
  test "fetching all habits from a new account returns an empty array" $ do
    void $ createRandomAccount >>= runClient <<< getHabits
  suite "putHabit" $ do
    test "putting a habit and then fetching it returns the habit" $ do
      session_info ← createRandomAccount
      retrieved_habit ← runClient $ do
        putHabit session_info test_habit_id test_habit >>= liftAff <<< equal HabitCreated
        getHabit session_info test_habit_id
      equal test_habit retrieved_habit
    test "putting a habit causes fetching all habits to return a singleton map" $ do
      session_info ← createRandomAccount
      runClient $ do
        putHabit session_info test_habit_id test_habit >>= liftAff <<< equal HabitCreated
        getHabits session_info >>= liftAff <<< equal (singleton test_habit_id test_habit)
    test "putting a habit, replacing it, and then fetching all habits returns the replaced habit" do
      session_info ← createRandomAccount
      runClient $ do
        putHabit session_info test_habit_id test_habit >>= liftAff <<< equal HabitCreated
        putHabit session_info test_habit_id test_habit_2 >>= liftAff <<< equal HabitReplaced
        getHabits session_info >>= liftAff <<< equal (singleton test_habit_id test_habit_2)
  suite "deleteHabit" $ do
    test "deleting a non-existing habit returns NoHabitToDelete" $ do
      session_info ← createRandomAccount
      runClient $
        deleteHabit session_info test_habit_id >>= liftAff <<< equal NoHabitToDelete
    test "putting a habit then deleting it causes fetching all habits to return an empty map" $ do
      session_info ← createRandomAccount
      runClient $ do
        putHabit session_info test_habit_id test_habit
        deleteHabit session_info test_habit_id >>= liftAff <<< equal HabitDeleted
        getHabits session_info >>= liftAff <<< equal empty
  test "marking habits should return the correct credits" $ do
      session_info ← createRandomAccount
      runClient $ do
        putHabit session_info test_habit_id test_habit >>= liftAff <<< equal HabitCreated
        putHabit session_info test_habit_id_2 test_habit_2 >>= liftAff <<< equal HabitCreated
        let expected_credits =
              Credits
                { success: (unwrap (unwrap test_habit).credits).success
                , failure: (unwrap (unwrap test_habit_2).credits).failure
                }
            marks =
              HabitsToMark
                { successes: [test_habit_id]
                , failures: [test_habit_id_2]
                }
        markHabits session_info marks >>= liftAff <<< equal expected_credits

