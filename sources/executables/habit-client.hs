{-# LANGUAGE UnicodeSyntax #-}

module Main where

import HabitOfFate.Prelude

import Control.Exception
import Options.Applicative
import System.Exit
import System.IO
import System.IO.Error (isEOFError)

import HabitOfFate.Client
  ( Cancel(..)
  , IOFunctions(..)
  , configuration_parser
  , runWithConfiguration
  )

getInput ∷ IO α → IO α
getInput getter =
  handleJust
    (\e →
      (
        case fromException e of
          Just UserInterrupt → Just $ putStrLn "" >> throwIO Cancel
          _ → Nothing
      )
      <|>
      (
        isEOFError <$> fromException e
        >>=
        bool Nothing (Just $ putStrLn "" >> exitSuccess)
      )
    )
    identity
    getter

io_functions ∷ IOFunctions
io_functions = IOFunctions
  { ioCancelFn = throwIO Cancel
  , ioGetCharFn = do
      c ← getInput $
        bracket_
          (hSetBuffering stdin NoBuffering)
          (hSetBuffering stdin LineBuffering)
          getChar
      unless (c ∈ "\r\n") $ putStrLn ""
      pure c
  , ioGetStrFn = getInput getLine
  , ioGetStrNoEchoFn = getInput $
      bracket_
        (hSetEcho stdout False)
        (hSetEcho stdout True)
        getLine
  , ioPutStrFn = \message → putStr message >> hFlush stdout
  , ioPutStrLnFn = \message → putStrLn message
  }

main ∷ IO ()
main =
  (
    execParser $ info
      (configuration_parser <**> helper)
      (   fullDesc
       <> header "habit-client - a client program for habit-of-fate"
      )
  )
  >>=
  runWithConfiguration io_functions
