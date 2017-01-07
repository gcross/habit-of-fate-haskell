{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bool
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text.IO as S
import Data.UUID (UUID)
import System.IO
import System.IO.Error (isEOFError)
import Text.Printf
import Text.Read (readEither, readMaybe)

import HabitOfFate.Client
import HabitOfFate.Habit
import HabitOfFate.Unicode

data Quit = Quit

newtype ActionMonad α = ActionMonad
  { unwrapActionMonad ∷ ExceptT Quit Client α
  } deriving (Applicative, Functor, Monad, MonadError Quit, MonadIO)

quit ∷ ActionMonad α
quit = throwError Quit

class Monad m ⇒ MonadClient m where
  liftC ∷ Client α → m α

instance MonadClient ActionMonad where
  liftC = ActionMonad ∘ lift

data Action = Action
  { _description ∷ String
  , _code ∷ ActionMonad ()
  }
makeLenses ''Action

type ActionMap = [(Char,Action)]

help ∷ MonadIO m ⇒ ActionMap → m ()
help commands = liftIO $ do
  putStrLn "Commands:"
  forM_ commands $ \(command, action) →
    printf "  %c: %s\n" command (action ^. description)
  putStrLn "  --"
  putStrLn "  q: Quit this menu."
  putStrLn "  ?: Display this help message."

data Cancel = Cancel

type ActionMonadWithCancel = ExceptT Cancel ActionMonad

instance MonadClient ActionMonadWithCancel where
  liftC = lift ∘ liftC

cancel ∷ ActionMonadWithCancel α
cancel = liftIO (putStrLn "") >> throwError Cancel

prompt ∷  String → ActionMonadWithCancel String
prompt p = join ∘ liftIO $ do
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

parseUUID ∷ String → Maybe UUID
parseUUID = readMaybe

parseUUIDs ∷ String → Maybe [UUID]
parseUUIDs =
    sequence
    ∘
    map parseUUID
    ∘
    split1
  where
    isSep = flip elem (" ," :: String)

    split1 = split2 ∘ dropWhile isSep

    split2 [] = []
    split2 x = entry:split1 rest
      where
        (entry,rest) = break isSep x

promptAndParse ∷ (String → Maybe α) → String → ActionMonadWithCancel α
promptAndParse parse p =
  prompt p
  >>=
  maybe
    (liftIO (putStrLn "Bad input.") >> promptAndParse parse p)
    return
  ∘
  parse

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

promptWithDefault ∷  String → String → ActionMonadWithCancel String
promptWithDefault def p =
  prompt (printf "%s [%s]" p def)
  <&>
  (\input → if null input then def else input)

promptWithDefault' ∷ (Read α, Show α) ⇒ α → String → ActionMonadWithCancel α
promptWithDefault' def p = doPrompt
  where
    doPrompt =
      promptWithDefault (show def) p
      >>=
      handleParseResult . readEither
    handleParseResult (Left e) = liftIO (putStrLn e) >> doPrompt
    handleParseResult (Right x) = return x

unrecognizedCommand ∷ MonadIO m ⇒ Char → m ()
unrecognizedCommand command
  | not (isAlpha command) = return ()
  | otherwise = liftIO $
      printf "Unrecognized command '%c'.  Press ? for help.\n" command

loop ∷ [String] → ActionMap → ActionMonad ()
loop labels commands = go
  where
    go =
      (promptForCommand $ printf "%s[%sq?]>"
        (intercalate "|" ("HoF":labels))
        (map fst commands)
      )
      >>=
      \command →
        fromMaybe (unrecognizedCommand command >> go)
        ∘
        lookup command
        $
        (chr 4, quit)
        :
        (chr 27, return ())
        :
        ('q',return ())
        :
        ('?',help commands >> go)
        :
        map (_2 %~ (>> go) ∘ view code) commands

withCancel ∷ ActionMonadWithCancel () → ActionMonad ()
withCancel = void ∘ runExceptT

printAndCancel = (>> cancel) ∘ liftIO ∘ putStrLn

mainLoop ∷ ActionMonad ()
mainLoop = loop [] $
  [('h',) ∘ Action "Edit habits." ∘ loop ["Habits"] $
    [('a',) ∘ Action "Add a habit." ∘ withCancel $ do
      Habit
        <$> prompt "What is the name of the habit?"
        <*> promptWithDefault' 1.0 "How many credits is a success worth?"
        <*> promptWithDefault' 0.0 "How many credits is a failure worth?"
      >>=
      void ∘ liftC ∘ createHabit
    ,('e',) ∘ Action "Edit a habit." ∘ withCancel $ do
      habit_id ← promptAndParse parseUUID "Which habit?"
      old_habit ←
        liftC (fetchHabit habit_id)
        >>=
        maybe
          (printAndCancel "No such habit.")
          return
      Habit
        <$> promptWithDefault (old_habit ^. name) "What is the name of the habit?"
        <*> promptWithDefault' (old_habit ^. success_credits) "How many credits is a success worth?"
        <*> promptWithDefault' (old_habit ^. failure_credits) "How many credits is a failure worth?"
       >>=
       liftC ∘ replaceHabit habit_id
    ,('f',) ∘ Action "Mark habits as failed." $
       withCancel $ promptAndParse parseUUIDs "Which habits failed?" >>= liftC ∘ markHabits []
    ,('p',) ∘ Action "Print habits." $ printHabits
    ,('s',) ∘ Action "Mark habits as successful." $
       withCancel $ promptAndParse parseUUIDs "Which habits succeeded?" >>= liftC ∘ flip markHabits []
    ]
  ,('p',) ∘ Action "Print data." $ do
      liftIO $ putStrLn "Habits:"
      printHabits
      liftIO $ putStrLn ""
      liftIO $ putStrLn "Game:"
      (success_credits, failure_credits) ← liftC getCredits
      liftIO $ printf "    Success credits: %f\n" success_credits
      liftIO $ printf "    Failure credits: %f\n" failure_credits
  ,('r',) ∘ Action "Run game." $
      liftC runGame >>= liftIO ∘ S.putStrLn
  ]
  where
    printHabits = do
      habits ← liftC fetchHabits
      liftIO $
        if Map.null habits
          then putStrLn "There are no habits."
          else forM_ (Map.toList habits) $ \(uuid, habit) →
            printf "%s %s [+%f/-%f]\n"
              (show uuid)
              (habit ^. name)
              (habit ^. success_credits)
              (habit ^. failure_credits)

main ∷ IO ()
main =
  void
  ∘
  flip runReaderT (ServerInfo "localhost" 8081)
  ∘
  runExceptT
  ∘
  unwrapActionMonad
  $
  mainLoop
