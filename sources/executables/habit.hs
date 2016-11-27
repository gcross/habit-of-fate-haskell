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
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Data.Yaml
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
  } deriving (Applicative, Functor, Monad, MonadIO)

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

prompt ∷ MonadIO m ⇒ String → m String
prompt p = liftIO $ do
  putStr p
  putStr " > "
  hFlush stdout
  getLine

promptForCredits :: MonadIO m ⇒ Int → String → m Int
promptForCredits def p = liftIO $ go
  where
    go = do
      input ← prompt $ printf "%s [%f]" p (fromIntegral def / 100 :: Float)
      case (readMaybe input :: Maybe Float) of
        Nothing
          | null input → return def
          | otherwise → printf "Invalid number: %s\n" input >> go
        Just n →
          let (number_of_credits, fraction) = properFraction $ n * 100
          in if fraction == 0
             then return number_of_credits
             else putStrLn "Number must not have more than two decimals." >> go

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

quit :: ActionMonad ()
quit = ActionMonad $ asks snd >>= lift ∘ ($ ())

unrecognizedCommand ∷ MonadIO m ⇒ Char → m ()
unrecognizedCommand command
  | not (isAlpha command) = return ()
  | otherwise = liftIO $
      printf "Unrecognized command '%c'.  Press ? for help." command

loop ∷ [String] → ActionMap → ActionMonad ()
loop labels commands = go
  where
    go =
      promptForCommand (printf "%s [%sq?] > " (intercalate " | " labels ) (map fst commands))
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

mainLoop :: ActionMonad ()
mainLoop = loop ["HabitOfFate"] $
  [('e', Action "Edit behaviors." editLoop)]
  where
    editLoop = loop ["HabitOfFate","Edit"] $
      [('h', Action "Edit habits." habitLoop)]
      where
        habitLoop = loop ["HabitOfFate","Edit","Habits"] $
          [('a', Action "Add a habit." addHabit)]
          where
            addHabit =
              Habit
                <$> prompt "What is the name of the habit?"
                <*> promptForCredits 100 "How many credits is a success worth?"
                <*> promptForCredits 0 "How many credits is a failure worth?"
              >>=
              (behaviors . habits %=) ∘ flip (⊞) ∘ (:[])

main :: IO ()
main = do
  filepath ← getArgs >>= \case
    [] → getHomeDirectory <&> (</> ".habit")
    [filepath] → return filepath
    _ → error "Only one argument may be provided."
  old_data ← doesFileExist filepath >>= \case
    True → BS.readFile filepath >>= either error return . decodeEither
    False → return newData
  new_data_ref ← newIORef old_data
  flip runContT return . callCC $ runReaderT (unwrapActionMonad mainLoop) . (new_data_ref,)
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
