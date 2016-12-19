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

import Prelude hiding (id)

import Control.Exception
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bool
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Data.UUID ()
import System.Console.ANSI
import System.Directory
import System.IO
import System.Random
import Text.Printf
import Text.Read (readEither, readMaybe)

import HabitOfFate.Behaviors
import HabitOfFate.Behaviors.Habit
import qualified HabitOfFate.Behaviors.Habit as Habit
import HabitOfFate.Console
import HabitOfFate.Data
import HabitOfFate.Game (GameState, belief)
import qualified HabitOfFate.Game as Game
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

help ∷ MonadIO m ⇒ ActionMap → m ()
help commands = liftIO $ do
  putStrLn "Commands:"
  forM_ commands $ \(command, action) →
    printf "  %c: %s\n" command (action ^. description)
  putStrLn "  --"
  putStrLn "  q: Quit this menu."
  putStrLn "  ?: Display this help message."

type CtrlC = () → ActionMonad ()

callCtrlC ∷ CtrlC → ActionMonad α
callCtrlC ctrl_c = liftIO (putStrLn "") >> ctrl_c () >> undefined

prompt ∷ CtrlC → String → ActionMonad String
prompt ctrl_c p =
  (liftIO $ do
    putStr p
    putChar ' '
    hFlush stdout
    handleJust
      (\case
        UserInterrupt → Just ()
        _ → Nothing
      )
      (const $ return Nothing)
      (Just <$> getLine)
  )
  >>=
  maybe (callCtrlC ctrl_c) return

parseNumberOrRepeat ∷ ActionMonad Int → Int → String → ActionMonad Int
parseNumberOrRepeat repeat top input =
  case (readMaybe input ∷ Maybe Int) of
    Nothing → liftIO (printf "Invalid number: %s\n" input) >> repeat
    Just n
      | 1 ≤ n && n ≤ top → return (n-1)
      | otherwise → liftIO (putStrLn "Out of range.") >> repeat

promptForIndex ∷ CtrlC → Int → String → ActionMonad Int
promptForIndex ctrl_c top p =
  callCC (\quit →
    prompt ctrl_c (printf "%s [1-%i]" p top)
    >>=
    (Just <$>) ∘ parseNumberOrRepeat (quit Nothing) top
  )
  >>=
  maybe (promptForIndex ctrl_c top p) return

promptForIndices ∷ CtrlC → Int → String → ActionMonad [Int]
promptForIndices ctrl_c top p =
  callCC (\quit →
    prompt ctrl_c (printf "%s [1-%i]" p top)
    >>=
    (Just <$>) ∘ mapM (parseNumberOrRepeat (quit Nothing) top) ∘ splitEntries
  )
  >>=
  maybe (promptForIndices ctrl_c top p) return
  where
    isSeparator ' ' = True
    isSeparator ',' = True
    isSeparator _ = False

    splitEntries [] = []
    splitEntries entries = entry:splitEntries (dropWhile isSeparator rest)
      where
        (entry,rest) = break isSeparator entries

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

promptWithDefault ∷ CtrlC → String → String → ActionMonad String
promptWithDefault ctrl_c def p =
  prompt ctrl_c (printf "%s [%s]" p def)
  <&>
  (\input → if null input then def else input)

promptWithDefault' ∷ (Read α, Show α) ⇒ CtrlC → α → String → ActionMonad α
promptWithDefault' ctrl_c def p = doPrompt
  where
    doPrompt =
      promptWithDefault ctrl_c (show def) p
      >>=
      handleParseResult . readEither
    handleParseResult (Left e) = liftIO (putStrLn e) >> doPrompt
    handleParseResult (Right x) = return x

quit ∷ ActionMonad ()
quit = ActionMonad $ asks snd >>= lift ∘ ($ ())

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

getNumberOfHabits ∷ ActionMonad Int
getNumberOfHabits = length <$> use (behaviors . habits)

gameStillHasCredits =
  (||)
    <$> ((/= 0) <$> use (game . Game.success_credits))
    <*> ((/= 0) <$> use (game . Game.failure_credits))

mainLoop ∷ ActionMonad ()
mainLoop = loop [] $
  [('h',) ∘ Action "Edit habits." ∘ loop ["Habits"] $
    [('a',) ∘ Action "Add a habit." $
      (callCC $ \ctrl_c →
        Habit
          <$> liftIO randomIO
          <*> prompt ctrl_c "What is the name of the habit?"
          <*> promptWithDefault' ctrl_c 1.0 "How many credits is a success worth?"
          <*> promptWithDefault' ctrl_c 0.0 "How many credits is a failure worth?"
        >>=
        ((behaviors . habits %=) ∘ flip (⊞) ∘ (:[]))
      )
    ,('e',) ∘ Action "Edit a habit." $
      (callCC $ \ctrl_c → do
        number_of_habits ← getNumberOfHabits
        abortIfNoHabits ctrl_c number_of_habits
        index ← promptForIndex ctrl_c number_of_habits "Which habit?"
        old_habit ←  fromJust <$> preuse (behaviors . habits . ix index)
        new_habit ←
          Habit
            <$> (return $ old_habit ^. id)
            <*> promptWithDefault ctrl_c (old_habit ^. name) "What is the name of the habit?"
            <*> promptWithDefault' ctrl_c (old_habit ^. success_credits) "How many credits is a success worth?"
            <*> promptWithDefault' ctrl_c (old_habit ^. failure_credits) "How many credits is a failure worth?"
        behaviors . habits . ix index .= new_habit
      )
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
          callCC $ \quit → forever $ do
            d ← get
            (paragraphs, completed, new_d) ← liftIO $ runData d
            put new_d
            liftIO $ do
              printParagraphs paragraphs
            gameStillHasCredits
              >>=
              bool
                (quit ())
                (do
                  liftIO $ do
                    putStrLn ""
                    pressAnyKeyToContinue
                    if completed
                      then do
                        putStrLn $ replicate 80 '='
                        putStrLn "A new quest begins..."
                        putStrLn $ replicate 80 '='
                      else do
                        putStrLn $ replicate 80 '-'
                    putStrLn ""
                    pressAnyKeyToContinue
                )
          liftIO $ putStrLn ""
        )
  ]
  where
    abortIfNoHabits ctrl_c number_of_habits =
      when (number_of_habits == 0) $ do
        liftIO $ putStrLn "There are no habits."
        ctrl_c ()

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
      habits' ← use (behaviors . habits)
      if null habits'
        then liftIO $ putStrLn "There are no habits."
        else forM_ (zip [1..] habits') $
          liftIO
          ∘
          (\(n,habit) → do
            printf "%i. " (n ∷ Int)
            printHabit habit
          )

    markHabits ∷ String → Lens' Habit Double → Lens' GameState Double → ActionMonad ()
    markHabits name habit_credits game_credits =
      (callCC $ \ctrl_c → do
        number_of_habits ← getNumberOfHabits
        abortIfNoHabits ctrl_c number_of_habits
        indices ← promptForIndices ctrl_c number_of_habits "Which habits?"
        old_success_credits ← use $ game . game_credits
        forM_ indices $ \index →
          preuse (behaviors . habits . ix index . habit_credits)
          >>=
          (game . game_credits +=) ∘ fromJust
        new_success_credits ← use $ game . game_credits
        liftIO $
          printf "%s credits went from %f to %f\n"
            name
            old_success_credits
            new_success_credits
      )

main ∷ IO ()
main = do
  filepath ← getDataFilePath
  old_data ←
    doesFileExist filepath
    >>=
    bool (return newData)
         (readData filepath)
  let run current_data = do
        new_data_ref ← newIORef current_data
        flip runContT return
          ∘
          callCC
          $
          runReaderT (unwrapActionMonad mainLoop)
          ∘
          (new_data_ref,)
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
