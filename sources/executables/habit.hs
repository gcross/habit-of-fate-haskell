{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Exception
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Data.Yaml hiding ((.=))
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Text.Printf
import Text.Read (readMaybe)

import HabitOfFate.Behaviors
import HabitOfFate.Behaviors.Habit
import HabitOfFate.Data
import HabitOfFate.Unicode

newtype ActionMonad α = ActionMonad
  { unwrapActionMonad ∷ ReaderT (IORef Data, () → ContT () IO ()) (ContT () IO) α
  } deriving (Applicative, Functor, Monad, MonadCont, MonadIO)

instance MonadState Data ActionMonad where
  get = ActionMonad $ asks fst >>= liftIO . readIORef
  put x = ActionMonad $ asks fst >>= liftIO . flip writeIORef x

data Action = Action
  { _description ∷ String
  , _code ∷ ActionMonad ()
  }
makeLenses ''Action

type ActionMap = [(Char,Action)]

formatCredits ∷ Int → String
formatCredits credits = printf "%.2f" (fromIntegral credits / 100 ∷ Double)

help ∷ MonadIO m ⇒ ActionMap → m ()
help commands = liftIO $ do
  putStrLn "Commands:"
  forM_ commands $ \(command, action) →
    printf "  %c: %s\n" command (action ^. description)
  putStrLn "  --"
  putStrLn "  q: Quit this menu."
  putStrLn "  ?: Display this help message."

data PromptError = PromptError

prompt ∷ (MonadError PromptError m, MonadIO m) ⇒ String → m String
prompt p =
  (liftIO $ do
    putStr p
    hFlush stdout
    tryJust
      (\case
        UserInterrupt → Just PromptError
        _ → Nothing
      )
      getLine
  )
  >>=
  either throwError return

parseNumberAndQuitLoop quitLoop top input =
  case (readMaybe input ∷ Maybe Int) of
    Nothing → liftIO (printf "Invalid number: %s\n" input)
    Just n
      | 1 ≤ n && n ≤ top → quitLoop n
      | otherwise → liftIO (putStrLn "Out of range.")

promptForIndex top p = callCC $ \quitLoop → forever $
  prompt (printf "%s [1-%i]" p top)
  >>=
  parseNumberAndQuitLoop quitLoop top

promptForCredits ∷ (MonadError PromptError m, MonadIO m) ⇒ Int → String → m Int
promptForCredits def p = do
  let repeat = promptForCredits def p
  input ← promptWithDefault (formatCredits def) p
  case (readMaybe input ∷ Maybe Float) of
    Nothing → liftIO (printf "Invalid number: %s\n" input) >> repeat
    Just n
      | n > 100 → liftIO (putStrLn "Number must not be larger than 100.") >> repeat
      | otherwise →
        let (number_of_credits, fraction) = properFraction $ n * 100
        in if fraction == 0
            then return number_of_credits
            else liftIO (putStrLn "Number must not have more than two decimals.") >> repeat

promptForCommand ∷ MonadIO m ⇒ String → m Char
promptForCommand p =
  liftIO
  .
  bracket_
    (hSetBuffering stdin NoBuffering)
    (hSetBuffering stdin LineBuffering)
  $ do
  putStr p
  hFlush stdout
  command ← getChar
  putStrLn ""
  return command

promptWithDefault ∷ (MonadError PromptError m, MonadIO m) ⇒ String → String → m String
promptWithDefault def p =
  prompt (printf "%s [%s]" p def)
  <&>
  (\input → if null input then def else input)

quit ∷ ActionMonad ()
quit = ActionMonad $ lift (asks snd) >>= lift ∘ lift ∘ ($ ())

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

printHabit = printf "%s [+%s/-%s]\n"
  <$> (^. name)
  <*> formatCredits ∘ (^. success_credits)
  <*> formatCredits ∘ (^. failure_credits)

mainLoop ∷ ActionMonad ()
mainLoop = loop [] $
  [('h',) ∘ Action "Edit habits." ∘ loop ["Habits"] $
    [('a',) ∘ Action "Add a habit." $
      runExceptT (
          Habit
            <$> prompt "What is the name of the habit?"
            <*> promptForCredits 100 "How many credits is a success worth?"
            <*> promptForCredits 0 "How many credits is a failure worth?"
      )
      >>=
      either
        (const ∘ liftIO $ putStrLn "")
        ((behaviors . habits %=) ∘ flip (⊞) ∘ (:[]))
    ,('e',) ∘ Action "Edit a habit." $
      runExceptT (do
        number_of_habits ← length <$> use (behaviors . habits)
        index ← subtract 1 <$> promptForIndex number_of_habits "Which habit?"
        old_habit ← (!! index) <$> use (behaviors . habits)
        new_habit ←
          Habit
            <$> promptWithDefault (old_habit ^. name) "What is the name of the habit?"
            <*> promptForCredits (old_habit ^. success_credits) "How many credits is a success worth?"
            <*> promptForCredits (old_habit ^. failure_credits) "How many credits is a failure worth?"
        behaviors . habits . ix index .= new_habit
      )
      >>=
      either
        (const ∘ liftIO $ putStrLn "")
        return
    ,('p',) ∘ Action "Print habits." $
      use (behaviors . habits)
      >>=
      mapM_ (
        liftIO
        ∘
        (\(n,habit) → do
          printf "%i. " (n ∷ Int)
          printHabit habit
        )
      )
      ∘
      zip [1..]
    ]
  ]

main ∷ IO ()
main = do
  filepath ← getArgs >>= \case
    [] → getHomeDirectory <&> (</> ".habit")
    [filepath] → return filepath
    _ → error "Only one argument may be provided."
  old_data ← doesFileExist filepath >>= \case
    True → BS.readFile filepath >>= either error return . decodeEither
    False → return newData
  new_data_ref ← newIORef old_data
  result_or_error ←
    flip runContT return
    ∘
    callCC
    ∘
    runReaderT (
      runExceptT
      ∘
      unwrapActionMonad 
      $
      mainLoop
    )
    ∘
    (new_data_ref,)
  new_data ← readIORef new_data_ref
  when (new_data /= old_data) $
    let go =
          promptForCommand "Save changes? [yn] > "
          >>=
          \case
            'y' → encodeFile filepath new_data
            'n' → return ()
            _ → putStrLn "Please type either 'y' or 'n'." >> go
    in go
