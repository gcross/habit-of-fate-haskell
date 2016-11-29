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

type CtrlC = () → ActionMonad ()

callCtrlC ∷ CtrlC → ActionMonad α
callCtrlC ctrl_c = liftIO (putStrLn "") >> ctrl_c () >> undefined

prompt ∷ CtrlC → String → ActionMonad String
prompt ctrl_c p =
  (liftIO $ do
    putStr p
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

promptForCredits ∷ CtrlC → Int → String → ActionMonad Int
promptForCredits ctrl_c def p = do
  let repeat = promptForCredits ctrl_c def p
  input ← promptWithDefault ctrl_c (formatCredits def) p
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

promptWithDefault ∷ CtrlC → String → String → ActionMonad String
promptWithDefault ctrl_c def p =
  prompt ctrl_c (printf "%s [%s]" p def)
  <&>
  (\input → if null input then def else input)

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

printHabit = printf "%s [+%s/-%s]\n"
  <$> (^. name)
  <*> formatCredits ∘ (^. success_credits)
  <*> formatCredits ∘ (^. failure_credits)

getNumberOfHabits ∷ ActionMonad Int
getNumberOfHabits = length <$> use (behaviors . habits)

mainLoop ∷ ActionMonad ()
mainLoop = loop [] $
  [('h',) ∘ Action "Edit habits." ∘ loop ["Habits"] $
    [('a',) ∘ Action "Add a habit." $
      (callCC $ \ctrl_c →
        Habit
          <$> prompt ctrl_c "What is the name of the habit?"
          <*> promptForCredits ctrl_c 100 "How many credits is a success worth?"
          <*> promptForCredits ctrl_c 0 "How many credits is a failure worth?"
        >>=
        ((behaviors . habits %=) ∘ flip (⊞) ∘ (:[]))
      )
    ,('e',) ∘ Action "Edit a habit." $
      (callCC $ \ctrl_c → do
        number_of_habits ← getNumberOfHabits
        when (number_of_habits == 0) $ do
          liftIO $ putStrLn "There are no habits."
          ctrl_c ()
        index ← promptForIndex ctrl_c number_of_habits "Which habit?"
        old_habit ←  fromJust <$> preuse (behaviors . habits . ix index)
        new_habit ←
          Habit
            <$> promptWithDefault ctrl_c (old_habit ^. name) "What is the name of the habit?"
            <*> promptForCredits ctrl_c (old_habit ^. success_credits) "How many credits is a success worth?"
            <*> promptForCredits ctrl_c (old_habit ^. failure_credits) "How many credits is a failure worth?"
        behaviors . habits . ix index .= new_habit
      )
    ,('p',) ∘ Action "Print habits." $ do
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
          promptForCommand "Save changes? [yn] > "
          >>=
          \case
            'y' → encodeFile filepath new_data
            'n' → return ()
            _ → putStrLn "Please type either 'y' or 'n'." >> go
    in go
