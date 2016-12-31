{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad.Error.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bool
import Data.Char
import Data.IORef
import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.UUID ()
import System.Console.ANSI
import System.Directory
import System.IO
import System.IO.Error (isEOFError)
import System.Random
import Text.Megaparsec
import Text.Megaparsec.Lexer (integer)
import Text.Megaparsec.String
import Text.Printf
import Text.Read (readEither, readMaybe)

import HabitOfFate.Console
import HabitOfFate.Data
import HabitOfFate.Game (GameState, belief)
import qualified HabitOfFate.Game as Game
import HabitOfFate.Habit
import qualified HabitOfFate.Habit as Habit
import HabitOfFate.Unicode

data Quit = Quit

newtype ActionMonad α = ActionMonad
  { unwrapActionMonad ∷ ReaderT (IORef Data) (ExceptT Quit IO) α
  } deriving (Applicative, Functor, Monad, MonadError Quit, MonadIO)

quit ∷ ActionMonad α
quit = throwError Quit

instance MonadState Data ActionMonad where
  get = ActionMonad $ ask >>= liftIO . readIORef
  put x = ActionMonad $ ask >>= liftIO . flip writeIORef x

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

parseNumberInRange ∷ Int → Parser Int
parseNumberInRange top = do
  n ← fromInteger <$> integer <|> fail "Invalid integer."
  unless (1 ≤ n && n ≤ top) $ fail "Out of range."
  return (n-1)

parseNumbersInRange ∷ Int → Parser [Int]
parseNumbersInRange = flip sepBy (char ',') ∘ parseNumberInRange

promptAndParse ∷ Parser α → String → ActionMonadWithCancel α
promptAndParse parser p = go
  where
    go =
      prompt p
      >>=
      either
        (
          \msg →
            (
              liftIO
              ∘
              putStrLn
              ∘
              (\[DecFail x] → x)
              ∘
              Set.toList
              ∘
              errorCustom
              $
              msg
            )
          >>
          go
        )
        return
      ∘
      parse parser ""

promptForI ∷ (Int → Parser α) → Int → String → ActionMonadWithCancel α
promptForI parser top p =
  promptAndParse
    (parser top)
    (printf "%s [1-%i]" p top)

promptForIndex ∷ Int → String → ActionMonadWithCancel Int
promptForIndex = promptForI parseNumberInRange

promptForIndices ∷ Int → String → ActionMonadWithCancel [Int]
promptForIndices = promptForI parseNumbersInRange

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

promptWithDefault' ∷ (Read α, Show α) ⇒  α → String → ActionMonadWithCancel α
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

printHabit = printf "%s [+%f/-%f]\n"
  <$> (^. name)
  <*> (^. success_credits)
  <*> (^. failure_credits)

getNumberOfHabits = length <$> use habits

gameStillHasCredits =
  (||)
    <$> ((/= 0) <$> use (game . Game.success_credits))
    <*> ((/= 0) <$> use (game . Game.failure_credits))

withCancel ∷ ActionMonadWithCancel () → ActionMonad ()
withCancel = void ∘ runExceptT

mainLoop ∷ ActionMonad ()
mainLoop = loop [] $
  [('h',) ∘ Action "Edit habits." ∘ loop ["Habits"] $
    [('a',) ∘ Action "Add a habit." ∘ withCancel $
      Habit
        <$> liftIO randomIO
        <*> prompt "What is the name of the habit?"
        <*> promptWithDefault' 1.0 "How many credits is a success worth?"
        <*> promptWithDefault' 0.0 "How many credits is a failure worth?"
      >>=
      (habits %=) ∘ flip (|>)
    ,('e',) ∘ Action "Edit a habit." ∘ withCancel $ do
      number_of_habits ← getNumberOfHabits
      cancelIfNoHabits number_of_habits
      index ← promptForIndex number_of_habits "Which habit?"
      old_habit ←  fromJust <$> preuse (habits . ix index)
      new_habit ←
        Habit
          <$> (return $ old_habit ^. uuid)
          <*> promptWithDefault (old_habit ^. name) "What is the name of the habit?"
          <*> promptWithDefault' (old_habit ^. success_credits) "How many credits is a success worth?"
          <*> promptWithDefault' (old_habit ^. failure_credits) "How many credits is a failure worth?"
      habits . ix index .= new_habit
    ,('f',) ∘ Action "Mark habits as failed." $
       markHabits "Failure" Habit.failure_credits Game.failure_credits
    ,('p',) ∘ Action "Print habits." $ printHabits
    ,('s',) ∘ Action "Mark habits as successful." $
       markHabits "Success" Habit.success_credits Game.success_credits
    ]
  ,('p',) ∘ Action "Print data." $ do
      liftIO $ putStrLn "Habits:"
      printHabits
      liftIO $ putStrLn ""
      liftIO $ putStrLn "Game:"
      let printCredits name =
            use . (game .)
            >=>
            liftIO ∘ printf "    %s credits: %f\n" name
      printCredits "Success" Game.success_credits
      printCredits "Failure" Game.failure_credits
      use (game . belief) >>= liftIO . printf "    Belief: %i\n"
      liftIO $ putStrLn ""
      use quest >>= liftIO . putStrLn . show
  ,('r',) ∘ Action "Run game." $
      gameStillHasCredits
      >>=
      bool
        (liftIO $ putStrLn "No credits.")
        (do
          liftIO $ putStrLn ""
          let go = do
                r ← get <&> runData
                put $ r ^. new_data
                liftIO ∘ printParagraphs $ r ^. paragraphs
                gameStillHasCredits
                  >>=
                  bool
                    (return ())
                    (do
                      liftIO $ do
                        putStrLn ""
                        pressAnyKeyToContinue
                        if r ^. quest_completed
                          then do
                            putStrLn $ replicate 80 '='
                            putStrLn "A new quest begins..."
                            putStrLn $ replicate 80 '='
                          else do
                            putStrLn $ replicate 80 '-'
                        putStrLn ""
                        pressAnyKeyToContinue
                      go
                    )
          go
          liftIO $ putStrLn ""
        )
  ]
  where
    cancelIfNoHabits number_of_habits =
      when (number_of_habits == 0) $ do
        liftIO $ putStrLn "There are no habits."
        cancel

    getGameCreditsAsFloat =
      ((/ (100 ∷ Float)) ∘ fromIntegral <$>)
      ∘
      use
      ∘
      (game .)

    pressAnyKeyToContinue = do
      putStrLn "[Press any key to continue.]"
      bracket_
        (do hSetBuffering stdin NoBuffering
            hSetEcho stdin False
        )
        (do hSetBuffering stdin LineBuffering
            hSetEcho stdin True
        )
        (void getChar)
      cursorUpLine 1
      clearLine

    printHabits = do
      habits' ← use habits
      if Seq.null habits'
        then liftIO $ putStrLn "There are no habits."
        else forM_ (zip [1..] (toList habits')) $
          liftIO
          ∘
          (\(n,habit) → do
            printf "%i. " (n ∷ Int)
            printHabit habit
          )

    markHabits ∷ String → Lens' Habit Double → Lens' GameState Double → ActionMonad ()
    markHabits name habit_credits game_credits = withCancel $ do
      number_of_habits ← getNumberOfHabits
      cancelIfNoHabits number_of_habits
      indices ← promptForIndices number_of_habits "Which habits?"
      old_success_credits ← use $ game . game_credits
      forM_ indices $ \index →
        preuse (habits . ix index . habit_credits)
        >>=
        (game . game_credits +=) ∘ fromJust
      new_success_credits ← use $ game . game_credits
      liftIO $
        printf "%s credits went from %f to %f\n"
          name
          old_success_credits
          new_success_credits

main ∷ IO ()
main = do
  filepath ← getDataFilePath
  old_data ←
    doesFileExist filepath
    >>=
    bool newData
         (readData filepath)
  let run current_data = do
        new_data_ref ← newIORef current_data
        void
          ∘
          runExceptT
          ∘
          flip runReaderT new_data_ref
          ∘
          unwrapActionMonad
          $
          mainLoop
        new_data ← readIORef new_data_ref
        when (new_data /= old_data) $
          let go =
                promptForCommand "Save changes? [yna]"
                >>=
                \case
                  'y' → writeData filepath new_data
                  'n' → return ()
                  'a' → run new_data
                  _ → putStrLn "Please type either 'y', 'n', or 'a''." >> go
          in go
  run old_data
