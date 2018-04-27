{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Client
  ( Cancel(..)
  , Configuration(..)
  , IOFunctions(..)
  , configuration_parser
  , getSessionInfoFromUser
  , runClientWithConfiguration
  ) where

import HabitOfFate.Prelude hiding (argument)

import Control.Monad.Catch
import qualified Data.ByteString as BS
import Data.Typeable (Typeable)
import Data.UUID (UUID)
import Options.Applicative
  ( Parser
  , argument
  , auto
  , help
  , long
  , metavar
  , short
  , strArgument
  , switch
  , value
  )
import Rainbow.Translate
import System.Exit (exitFailure, exitSuccess)
import qualified System.IO as IO
import System.Random

import HabitOfFate.API
import HabitOfFate.Credits
import HabitOfFate.Habit
import HabitOfFate.Story

data IOFunctions = IOFunctions
  { ioGetCharFn ∷ IO Char
  , ioGetStrFn ∷ IO String
  , ioGetStrNoEchoFn ∷ IO String
  , ioPutStrFn ∷ String → IO ()
  , ioPutStrLnFn ∷ String → IO ()
  }

type InteractionMonad = ReaderT IOFunctions SessionIO

type InteractionMonadConstraints m =
  ( MonadIO m
  , MonadReader IOFunctions m
  , MonadThrow m
  )

ioGetChar ∷ InteractionMonadConstraints m ⇒ m Char
ioGetChar = asks ioGetCharFn >>= liftIO

ioGetStr ∷ InteractionMonadConstraints m ⇒ m String
ioGetStr = asks ioGetStrFn >>= liftIO

ioGetStrNoEcho ∷ InteractionMonadConstraints m ⇒ m String
ioGetStrNoEcho = asks ioGetStrNoEchoFn >>= liftIO

ioPutStr ∷ InteractionMonadConstraints m ⇒ String → m ()
ioPutStr message = do
  putStr ← asks ioPutStrFn
  liftIO $ putStr message

ioPutStrLn ∷ InteractionMonadConstraints m ⇒ String → m ()
ioPutStrLn message = do
  putStr ← asks ioPutStrLnFn
  liftIO $ putStr message

data Cancel = Cancel deriving (Show, Typeable)
instance Exception Cancel where

cancel ∷ InteractionMonadConstraints m ⇒ m α
cancel = throwM Cancel

liftSession ∷ SessionIO α → InteractionMonad α
liftSession = lift

data MenuAction =
    SubMenu String [MenuItem]
  | Interaction (InteractionMonad ())

data MenuItem = MenuItem
  { keyOf ∷ Char
  , descriptionOf ∷ String
  , actionOf ∷ MenuAction
  }

subMenu ∷ Char → String → String → Menu → MenuItem
subMenu key description sublabel subitems =
  MenuItem key description (SubMenu sublabel subitems)

interaction ∷ Char → String → InteractionMonad () → MenuItem
interaction key description interaction' =
  MenuItem key description (Interaction interaction')

type Menu = [MenuItem]

printHelp ∷ Menu → InteractionMonad ()
printHelp items = do
  ioPutStrLn "Interactions:"
  forM_ items $ \item →
    ioPutStrLn [i|  #{keyOf item:""}: #{descriptionOf item}|]
  ioPutStrLn "  --"
  ioPutStrLn "  q: Quit this menu."
  ioPutStrLn "  ?: Display this help message."

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

prompt ∷ (String → Maybe α) → String → InteractionMonad α
prompt parseValue p =
  promptForString
  >>=
  (
    parseValue
    >>>
    maybe (ioPutStrLn "Bad input." >> prompt parseValue p) return
  )
  where
    promptForString = ioPutStr (p ⊕ " ") >> ioGetStr

promptWithDefault ∷ Show α ⇒ (String → Maybe α) → α → String → InteractionMonad α
promptWithDefault parseValue default_value p =
    prompt parseValue' [i|#{p} [#{show default_value}]|]
  where
    parseValue' "" = Just default_value
    parseValue' x = parseValue x

unrecognizedCommand ∷ Char → InteractionMonad ()
unrecognizedCommand ' ' = pure ()
unrecognizedCommand command =
  ioPutStrLn [i|Unrecognized command #{command}.  Press ? for help.|]

runMenu ∷ [String] → Menu → InteractionMonad ()
runMenu labels items = go
  where
    action_map = map (keyOf &&& actionOf) items
    location = ointercalate "/" labels
    command_prompt = [i|#{location} [#{map keyOf items}q?] |]

    go = ioPutStr command_prompt >> ioGetChar >>= \case
      '\^D' → liftIO exitSuccess
      '\^[' → return () -- escape key
      'q' → return ()
      '?' → printHelp items >> go
      command → case lookup command action_map of
        Nothing → unrecognizedCommand command >> go
        Just (SubMenu sublabel subitems) → runMenu (labels ⊕ [sublabel]) subitems >> go
        Just (Interaction action) → (action `catch` (\Cancel → pure ())) >> go

printAndCancel ∷ String → InteractionMonad α
printAndCancel message = ioPutStrLn message >> cancel

main_menu ∷ Menu
main_menu =
  [subMenu 'h' "Edit habits." "Habits"
    [interaction 'a' "Add a habit." $ do
      habit_id ← liftIO randomIO
      habit ← Habit
        <$> prompt readNonEmpty "What is the name of the habit?"
        <*> promptWithDefault readMaybe Medium ("Importance " ⊕ scale_options)
        <*> promptWithDefault readMaybe Medium ("Difficulty " ⊕ scale_options)
      liftSession >>> void $ putHabit habit_id habit
      "Habit ID is " ⊕ show habit_id |> ioPutStrLn
    ,interaction 'e' "Edit a habit." $ do
      habit_id ← prompt readMaybe "Which habit?"
      old_habit ←
        liftSession (getHabit habit_id)
        >>=
        maybe
          (printAndCancel "No such habit.")
          return
      habit ← Habit
        <$> promptWithDefault readNonEmpty (old_habit ^. name) "What is the name of the habit?"
        <*> promptWithDefault readMaybe Medium ("Importance " ⊕ scale_options)
        <*> promptWithDefault readMaybe Medium ("Difficulty " ⊕ scale_options)
      liftSession >>> void $ putHabit habit_id habit
    ,interaction 'f' "Mark habits as failed." $
       prompt parseUUIDs "Which habits failed?" >>= (markHabits [] >>> liftSession >>> void)
    ,interaction 'p' "Print habits." $ printHabits
    ,interaction 's' "Mark habits as successful." $
       prompt parseUUIDs "Which habits succeeded?" >>= (flip markHabits [] >>> liftSession >>> void)
    ]
  ,interaction 'p' "Print data." $ do
      ioPutStrLn "Habits:"
      printHabits
      ioPutStrLn ""
      ioPutStrLn "Game:"
      credits ← liftSession getCredits
      ioPutStrLn [i|    Success credits: #{credits ^. success}|]
      ioPutStrLn [i|    Failure credits: #{credits ^. failure}|]
  ,interaction 'r' "Run game." $ do
      story ← liftSession runGame
      printer ← liftIO byteStringMakerFromEnvironment
      story
        |> renderStoryToChunks
        |> chunksToByteStrings printer
        |> traverse_ BS.putStr
        |> liftIO
  ]
  where
    scale_options = " [" ⊕ ointercalate ", " (map show [minBound..maxBound ∷ Scale]) ⊕ "]"

    printHabits = do
      habits ← liftSession getHabits
      if null habits
        then ioPutStrLn "There are no habits."
        else forM_ (mapToList habits) $ \(uuid, habit) →
          ioPutStrLn $
            printf "%s %s [+%s/-%s]"
              (show uuid)
              (habit ^. name)
              (show $ habit ^. difficulty)
              (show $ habit ^. importance)

runMainMenu ∷ InteractionMonad ()
runMainMenu = runMenu ["HoF"] main_menu

data Configuration = Configuration
  { hostname ∷ String
  , port ∷ Int
  , create_account_mode ∷ Bool
  }

configuration_parser ∷ Parser Configuration
configuration_parser = Configuration
  <$> strArgument (mconcat
        [ metavar "HOSTNAME"
        , help "Name of the host to connect to."
        , value "localhost"
        ])
  <*> argument auto (mconcat
        [ metavar "PORT"
        , help "Port to connect to."
        , value 8081
        ])
  <*> switch (mconcat
        [ help "Create a new account."
        , long "create"
        , short 'c'
        ])

getSessionInfoFromUser ∷ IOFunctions → Configuration → IO SessionInfo
getSessionInfoFromUser io_functions Configuration{..} = do
  (username, password) ← flip runReaderT io_functions $
    liftA2 (,)
      (ioPutStr "Username: " >> ioGetStr)
      (ioPutStr "Password: " >> ioGetStrNoEcho)
  let doLogin =
        login username password Secure "localhost" 8081
        >>=
        either
          (\err → do
            IO.putStrLn $
              case err of
                NoSuchAccount → "No such account."
                InvalidPassword → "Invalid password."
            liftIO exitFailure
          )
          pure
  if create_account_mode
    then
      createAccount username password Secure "localhost" 8081
      >>=
      maybe
        (IO.putStrLn "Account already exists. Logging in..." >> doLogin)
        pure
    else doLogin

runClientWithConfiguration ∷ IOFunctions → Configuration → IO ()
runClientWithConfiguration io_functions = do
  getSessionInfoFromUser io_functions
  >=>
  runSessionT (runReaderT runMainMenu io_functions)
