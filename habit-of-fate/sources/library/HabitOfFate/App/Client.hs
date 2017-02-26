{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.App.Client where

import HabitOfFate.Prelude

import Control.Exception (AsyncException(UserInterrupt))
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Cont
import Control.Monad.Trans.Control
import qualified Data.ByteString as BS
import Data.Char
import qualified Data.Text.IO as S
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Rainbow.Translate
import System.IO
import System.IO.Error (isEOFError)
import System.Random
import Text.Read (readEither, readMaybe)
import Text.XML

import HabitOfFate.Client
import HabitOfFate.Credits
import HabitOfFate.Habit
import HabitOfFate.Story

data Quit = Quit

type InnerAction = ExceptT Quit Client

newtype ActionMonad α = ActionMonad
  { unwrapActionMonad ∷ InnerAction α }
  deriving
  ( Applicative
  , Functor
  , Monad
  , MonadBase IO
  , MonadCatch
  , MonadError Quit
  , MonadIO
  , MonadThrow
  )

instance MonadBaseControl IO ActionMonad where
  type StM ActionMonad α = StM InnerAction α
  liftBaseWith f = ActionMonad $ liftBaseWith $ \r → f (r ∘ unwrapActionMonad)
  restoreM = ActionMonad ∘ restoreM

quit ∷ ActionMonad α
quit = throwError Quit

class Monad m ⇒ MonadClient m where
  liftC ∷ Client α → m α

instance MonadClient ActionMonad where
  liftC = ActionMonad ∘ lift

data Action = Action
  { _key ∷ Char
  , _description ∷ String
  , _code ∷ ActionMonad ()
  }
makeLenses ''Action

help ∷ MonadIO m ⇒ [Action] → m ()
help actions = liftIO $ do
  putStrLn "Actions:"
  forM_ actions $
    printf "  %c: %s\n" <$> (^. key) <*> (^. description)
  putStrLn "  --"
  putStrLn "  q: Quit this menu."
  putStrLn "  ?: Display this help message."

data Cancel = Cancel

type ActionMonadWithCancel = ExceptT Cancel ActionMonad

instance MonadClient ActionMonadWithCancel where
  liftC = lift ∘ liftC

cancel ∷ ActionMonadWithCancel α
cancel = liftIO (putStrLn "") >> throwError Cancel

class EditableValue α where
  showValue ∷ α → String
  default showValue ∷ Show α ⇒ α → String
  showValue = show

  parseValue ∷ String → Maybe α
  default parseValue ∷ Read α ⇒ String → Maybe α
  parseValue = readMaybe

instance EditableValue String where
  showValue = identity
  parseValue = Just

instance EditableValue Double where

instance EditableValue UUID where

instance EditableValue [UUID] where
  showValue = intercalate "," ∘ map showValue
  parseValue =
    sequence
    ∘
    map parseValue
    ∘
    split1
    where
      isSep = flip elem (" ," :: String)

      split1 = split2 ∘ dropWhile isSep

      split2 [] = []
      split2 x = entry:split1 rest
        where
          (entry,rest) = break isSep x

prompt ∷ EditableValue α ⇒ String → ActionMonadWithCancel α
prompt p =
  promptForString
  >>=
  maybe
    (liftIO (putStrLn "Bad input.") >> prompt p)
    return
  ∘
  parseValue
  where
    promptForString =
      join ∘ liftIO $ do
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
              bool Nothing (Just $ liftIO (putStrLn "") >> lift quit)
            )
          )
          return
          (return <$> getLine)

promptWithDefault ∷ EditableValue α ⇒ α → String → ActionMonadWithCancel α
promptWithDefault def p = doPrompt
  where
    doPrompt =
      prompt (printf "%s [%s]" p (showValue def))
      >>=
      (\input →
        if null input
          then return def
          else case parseValue input of
            Nothing → doPrompt
            Just value → return value
      )

promptForCommand ∷ MonadIO m ⇒ String → m Char
promptForCommand p =
  liftIO
  .
  bracket_
    (hSetBuffering stdin NoBuffering)
    (hSetBuffering stdin LineBuffering)
  $ do
  putStr p
  putChar ' '
  hFlush stdout
  command ← getChar
  putStrLn ""
  return command

unrecognizedCommand ∷ MonadIO m ⇒ Char → m ()
unrecognizedCommand command
  | not (isAlpha command) = return ()
  | otherwise = liftIO $
      printf "Unrecognized command '%c'.  Press ? for help.\n" command

data Escape = Escape

loop ∷ [String] → [Action] → ActionMonad ()
loop labels actions = void ∘ runExceptT $ do
  let action_map =
        [(chr 4, lift quit)
        ,(chr 27, throwError Escape)
        ,('q', throwError Escape)
        ,('?',help actions)
        ]
        ⊕
        (map ((^. key) &&& (^. code . to lift)) actions)
  forever $ do
    command ← promptForCommand $
      printf "%s[%sq?]>"
        (ointercalate "|" ("HoF":labels))
        (map (^. key) actions)
    case lookup command action_map of
      Nothing → unrecognizedCommand command
      Just code → code `catchAll` (liftIO ∘ putStrLn ∘ show)

withCancel ∷ ActionMonadWithCancel () → ActionMonad ()
withCancel = void ∘ runExceptT

printAndCancel = (>> cancel) ∘ liftIO ∘ putStrLn

mainLoop ∷ ActionMonad ()
mainLoop = loop [] $
  [Action 'h' "Edit habits." ∘ loop ["Habits"] $
    [Action 'a' "Add a habit." ∘ withCancel $ do
      habit_id ← liftIO randomIO
      habit ← Habit
        <$> prompt "What is the name of the habit?"
        <*> (Credits
              <$> promptWithDefault 1.0 "How many credits is a success worth?"
              <*> promptWithDefault 0.0 "How many credits is a failure worth?"
            )
      void ∘ liftC $ putHabit habit_id habit
    ,Action 'e' "Edit a habit." ∘ withCancel $ do
      habit_id ← prompt "Which habit?"
      old_habit ←
        liftC (fetchHabit habit_id)
        >>=
        maybe
          (printAndCancel "No such habit.")
          return
      habit ← Habit
        <$> promptWithDefault (old_habit ^. name) "What is the name of the habit?"
        <*> (Credits
             <$> promptWithDefault (old_habit ^. credits . success) "How many credits is a success worth?"
             <*> promptWithDefault (old_habit ^. credits . failure) "How many credits is a failure worth?"
            )
      void ∘ liftC $ putHabit habit_id habit
    ,Action 'f' "Mark habits as failed." $
       withCancel $ prompt "Which habits failed?" >>= void ∘ liftC ∘ markHabits []
    ,Action 'p' "Print habits." $ printHabits
    ,Action 's' "Mark habits as successful." $
       withCancel $ prompt "Which habits succeeded?" >>= void ∘ liftC ∘ flip markHabits []
    ]
  ,Action 'p' "Print data." $ do
      liftIO $ putStrLn "Habits:"
      printHabits
      liftIO $ putStrLn ""
      liftIO $ putStrLn "Game:"
      credits ← liftC getCredits
      liftIO $ printf "    Success credits: %f\n" (credits ^. success)
      liftIO $ printf "    Failure credits: %f\n" (credits ^. failure)
  ,Action 'r' "Run game." $ do
      story ← liftC runGame
      printer ← liftIO byteStringMakerFromEnvironment
      liftIO
        ∘
        traverse_ BS.putStr
        ∘
        chunksToByteStrings printer
        ∘
        renderStoryToChunks
        $
        story
  ]
  where
    printHabits = do
      habits ← liftC fetchHabits
      liftIO $
        if null habits
          then putStrLn "There are no habits."
          else forM_ (mapToList habits) $ \(uuid, habit) →
            printf "%s %s [+%f/-%f]\n"
              (show uuid)
              (habit ^. name)
              (habit ^. credits . success)
              (habit ^. credits . failure)

doMain ∷ IO ()
doMain = do
  putStr "Username: "
  hFlush stdout
  username ← getLine
  putStr "Password: "
  hFlush stdout
  hSetEcho stdout False
  password ← getLine
  hSetEcho stdout True
  putStrLn ""
  error_or_session_info ← login username password Secure "localhost" 8081
  case error_or_session_info of
    Left NoSuchAccount → putStrLn "No such account."
    Left InvalidPassword → putStrLn "Invalid password."
    Right session_info →
      void
      ∘
      flip runReaderT session_info
      ∘
      runExceptT
      ∘
      unwrapActionMonad
      $
      mainLoop
