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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Client (Configuration(..), doMain) where

import HabitOfFate.Prelude

import Control.Exception (AsyncException(UserInterrupt))
import Control.Monad.Catch
import qualified Data.ByteString as BS
import Data.Char
import Data.Typeable (Typeable)
import Data.UUID (UUID)
import Rainbow.Translate
import System.Exit (exitSuccess)
import System.IO
import System.IO.Error (isEOFError)
import System.Random

import HabitOfFate.API
import HabitOfFate.Credits
import HabitOfFate.Habit
import HabitOfFate.Story
import HabitOfFate.Story.Renderer.Console

data Cancel = Cancel deriving (Show, Typeable)
instance Exception Cancel where

cancel ∷ SessionIO α
cancel = liftIO (putStrLn "") >> throwM Cancel

data MenuAction =
    SubMenu String [MenuItem]
  | Interaction (SessionIO ())

data MenuItem = MenuItem
  { keyOf ∷ Char
  , descriptionOf ∷ String
  , actionOf ∷ MenuAction
  }

subMenu ∷ Char → String → String → Menu → MenuItem
subMenu key description sublabel subitems =
  MenuItem key description (SubMenu sublabel subitems)

interaction ∷ Char → String → SessionIO () → MenuItem
interaction key description interaction' =
  MenuItem key description (Interaction interaction')

type Menu = [MenuItem]

printHelp ∷ MonadIO m ⇒ Menu → m ()
printHelp items = liftIO $ do
  putStrLn "Interactions:"
  forM_ items $ \item →
    putStrLn [i|  #{keyOf item:""}: #{descriptionOf item}|]
  putStrLn "  --"
  putStrLn "  q: Quit this menu."
  putStrLn "  ?: Display this help message."

parseUUIDs ∷ String → Maybe [UUID]
parseUUIDs = split1 >>> map readMaybe >>> sequence
  where
    isSep = flip elem (" ," :: String)

    split1 = dropWhile isSep >>> split2

    split2 [] = []
    split2 x = entry:split1 rest
      where
        (entry,rest) = break isSep x

readNonEmpty ∷ String → Maybe String
readNonEmpty "" = Nothing
readNonEmpty x = Just x

prompt ∷ (String → Maybe α) → String → SessionIO α
prompt parseValue p =
  promptForString
  >>=
  (
    parseValue
    >>>
    maybe (liftIO (putStrLn "Bad input.") >> prompt parseValue p) return
  )
  where
    promptForString =
      liftIO >>> join $ do
        putStr p
        putChar ' '
        hFlush stdout
        handleJust
          (\e →
            (
              case fromException e of
                Just UserInterrupt → Just cancel
                _ → Nothing
            )
            <|>
            (
              isEOFError <$> fromException e
              >>=
              bool Nothing (Just $ liftIO (putStrLn "") >> liftIO exitSuccess)
            )
          )
          return
          (return <$> getLine)

promptWithDefault ∷ Show α ⇒ (String → Maybe α) → α → String → SessionIO α
promptWithDefault parseValue default_value p =
    prompt parseValue' [i|#{p} [#{show default_value}]|]
  where
    parseValue' "" = Just default_value
    parseValue' x = parseValue x

promptForCommand ∷ MonadIO m ⇒ String → m Char
promptForCommand p =
  (
    bracket_
      (hSetBuffering stdin NoBuffering)
      (hSetBuffering stdin LineBuffering)
    >>>
    liftIO
  )
  $
  do putStr p
     putChar ' '
     hFlush stdout
     command ← getChar
     putStrLn ""
     return command

unrecognizedCommand ∷ MonadIO m ⇒ Char → m ()
unrecognizedCommand command
  | not (isAlphaNum command) = return ()
  | otherwise = liftIO $
      putStrLn [i|Unrecognized command #{command}.  Press ? for help.|]

runMenu ∷ [String] → Menu → SessionIO ()
runMenu labels items = go
  where
    action_map = map (keyOf &&& actionOf) items
    location = ointercalate "/" labels
    command_prompt = [i|#{location} [#{map keyOf items}q?]|]

    go = promptForCommand command_prompt >>= \case
      '\^D' → liftIO exitSuccess
      '\^[' → return () -- escape key
      'q' → return ()
      '?' → printHelp items >> go
      command → case lookup command action_map of
        Nothing → unrecognizedCommand command >> go
        Just (SubMenu sublabel subitems) → runMenu (labels ⊕ [sublabel]) subitems >> go
        Just (Interaction action) → (action `catch` (\Cancel → pure ())) >> go

printAndCancel ∷ String → SessionIO α
printAndCancel = putStrLn >>> liftIO >>> (>> cancel)

main_menu ∷ Menu
main_menu =
  [subMenu 'h' "Edit habits." "Habits"
    [interaction 'a' "Add a habit." $ do
      habit_id ← liftIO randomIO
      habit ← Habit
        <$> (pack <$> prompt readNonEmpty "What is the name of the habit?")
        <*> (Difficulty <$> promptWithDefault readMaybe Medium ("Difficulty " ⊕ scale_options))
        <*> (Importance <$> promptWithDefault readMaybe Medium ("Importance " ⊕ scale_options))
      void $ putHabit habit_id habit
      "Habit ID is " ⊕ show habit_id |> putStrLn |> liftIO
    ,interaction 'e' "Edit a habit." $ do
      habit_id ← prompt readMaybe "Which habit?"
      old_habit ←
        getHabit habit_id
        >>=
        maybe
          (printAndCancel "No such habit.")
          return
      habit ← Habit
        <$> (pack <$>
               promptWithDefault readNonEmpty (old_habit ^. name_ . to unpack) "What is the name of the habit?")
        <*> (Difficulty <$> promptWithDefault readMaybe Medium ("Difficulty " ⊕ scale_options))
        <*> (Importance <$> promptWithDefault readMaybe Medium ("Importance " ⊕ scale_options))
      void $ putHabit habit_id habit
    ,interaction 'f' "Mark habits as failed." $
       prompt parseUUIDs "Which habits failed?" >>= (markHabits [] >>> void)
    ,interaction 'p' "Print habits." $ printHabits
    ,interaction 's' "Mark habits as successful." $
       prompt parseUUIDs "Which habits succeeded?" >>= (flip markHabits [] >>> void)
    ]
  ,interaction 'p' "Print data." $ do
      liftIO $ putStrLn "Habits:"
      printHabits
      liftIO $ putStrLn ""
      liftIO $ putStrLn "Game:"
      credits ← getCredits
      liftIO $ putStrLn [i|    Success credits: #{credits ^. successes_}|]
      liftIO $ putStrLn [i|    Failure credits: #{credits ^. failures_}|]
  ,interaction 'r' "Run game." $ getAndRunStory runGame
  ,interaction 's' "Get quest status." $ getAndRunStory getQuestStatus
  ]
  where
    scale_options = " [" ⊕ ointercalate ", " (map show [minBound..maxBound ∷ Scale]) ⊕ "]"

    printHabits = do
      habits_to_display ← getHabits <&> view habit_list_
      liftIO $
        if null habits_to_display
          then putStrLn "There are no habits."
          else forM_ habits_to_display $ \(uuid, habit) →
            printf "%s %s [+%s/-%s]\n"
              (show uuid)
              (habit ^. name_)
              (show $ habit ^. difficulty_)
              (show $ habit ^. importance_)

    getAndRunStory = (>>= (printEvent >>> liftIO))

data Configuration = Configuration
  { hostname ∷ String
  , port ∷ Int
  , create_account_mode ∷ Bool
  }

runSession ∷ SessionInfo → IO ()
runSession session_info =
  main_menu
    |> runMenu ["HoF"]
    |> flip runSessionT session_info
    |> void

doMain ∷ SecureMode → Configuration → IO ()
doMain secure_mode Configuration{..} = do
  putStr "Username: "
  hFlush stdout
  username ← getLine
  putStr "Password: "
  hFlush stdout
  hSetEcho stdout False
  password ← getLine
  hSetEcho stdout True
  putStrLn ""
  let doLogin =
        login username password secure_mode "localhost" 8081
        >>=
        either
          (\case
            NoSuchAccount → putStrLn "No such account."
            InvalidPassword → putStrLn "Invalid password."
          )
          runSession
  if create_account_mode
    then
      createAccount username password secure_mode "localhost" 8081
      >>=
      maybe
        (putStrLn "Account already exists. Logging in..." >> doLogin)
        runSession
    else doLogin
