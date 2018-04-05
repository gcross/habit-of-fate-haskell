{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Client (doMain) where

import HabitOfFate.Prelude hiding (argument)

import Control.Exception (AsyncException(UserInterrupt))
import Control.Monad.Catch
import qualified Data.ByteString as BS
import Data.Char
import Data.UUID (UUID)
import Options.Applicative
  ( Parser
  , argument
  , auto
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , short
  , strArgument
  , switch
  , value
  )
import Rainbow.Translate
import System.Exit (exitSuccess)
import System.IO
import System.IO.Error (isEOFError)
import System.Random

import HabitOfFate.API
import HabitOfFate.Credits
import HabitOfFate.Habit
import HabitOfFate.Story

data Cancel = Cancel

type InteractionMonad = ExceptT Cancel SessionIO

cancel ∷ InteractionMonad α
cancel = liftIO (putStrLn "") >> throwError Cancel

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

prompt ∷ (String → Maybe α) → String → InteractionMonad α
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

promptWithDefault ∷ Show α ⇒ (String → Maybe α) → α → String → InteractionMonad α
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
        Just (Interaction action) → void (runExceptT action) >> go

printAndCancel ∷ String → InteractionMonad α
printAndCancel = putStrLn >>> liftIO >>> (>> cancel)

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
      "Habit ID is " ⊕ show habit_id |> putStrLn |> liftIO
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
      liftIO $ putStrLn "Habits:"
      printHabits
      liftIO $ putStrLn ""
      liftIO $ putStrLn "Game:"
      credits ← liftSession getCredits
      liftIO $ putStrLn [i|    Success credits: #{credits ^. success}|]
      liftIO $ putStrLn [i|    Failure credits: #{credits ^. failure}|]
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
      liftIO $
        if null habits
          then putStrLn "There are no habits."
          else forM_ (mapToList habits) $ \(uuid, habit) →
            printf "%s %s [+%s/-%s]\n"
              (show uuid)
              (habit ^. name)
              (show $ habit ^. difficulty)
              (show $ habit ^. importance)

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

runSession ∷ SessionInfo → IO ()
runSession session_info =
  main_menu
    |> runMenu ["HoF"]
    |> flip runSessionT session_info
    |> void

doMain ∷ IO ()
doMain = do
  Configuration{..} ←
    execParser $ info
      (configuration_parser <**> helper)
      (   fullDesc
       <> header "habit-client - a client program for habit-of-fate"
      )
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
        login username password Secure "localhost" 8081
        >>=
        either
          (\case
            NoSuchAccount → putStrLn "No such account."
            InvalidPassword → putStrLn "Invalid password."
          )
          runSession
  if create_account_mode
    then
      createAccount username password Secure "localhost" 8081
      >>=
      maybe
        (putStrLn "Account already exists. Logging in..." >> doLogin)
        runSession
    else doLogin
