{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Console where

import Control.Lens ((%=),(.=),use)
import Control.Lens.TH (makeLenses)
import Control.Monad (forM_,replicateM_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Random (MonadRandom(), RandT(), evalRandT)
import Control.Monad.Trans.State.Strict (StateT(), evalStateT)
import Data.Char (isLower, toUpper)
import Data.List (span)
import System.Console.ANSI
    (Color(..)
    ,ColorIntensity(Dull,Vivid)
    ,ConsoleIntensity(BoldIntensity,NormalIntensity)
    ,ConsoleLayer(Foreground)
    ,SGR(SetConsoleIntensity,SetUnderlining,SetColor)
    ,Underlining(SingleUnderline,NoUnderline)
    ,setSGR
    )
import System.Random (StdGen, newStdGen)

import HabitOfFate.Game
import HabitOfFate.Unicode

data ColorSpec = ColorSpec ColorIntensity Color

parseColorSpec :: String → ColorSpec
parseColorSpec [hue] = ColorSpec intensity color
  where
    color = case toUpper hue of
        'K' → Black
        'R' → Red
        'G' → Green
        'Y' → Yellow
        'B' → Blue
        'M' → Magenta
        'C' → Cyan
        'W' → White
        _ → error $ "Bad color: " ++ [hue]
    intensity = if isLower hue then Dull else Vivid
parseColorSpec x = error $ "Bad color spec: " ++ x

countVisibleCharsIn :: String → Int
countVisibleCharsIn = go 0
  where
    go accum ('[':xs) = go2 xs
      where
        go2 ('|':ys) = go accum ys
        go2 (_:ys) = go2 ys
        go2 [] = error "No closing ]"
    go accum (x:xs)
      | x ∈ ['*','_',']'] = go accum xs
      | otherwise = go (accum+1) xs
    go accum [] = accum

data PrintTextState = PrintTextState
    { _current_columns :: Int
    , _currently_bold :: Bool
    , _currently_underlined :: Bool
    , _color_restorers :: [IO ()]
    }
makeLenses ''PrintTextState

printParagraphs :: [[String]] → IO ()
printParagraphs [] = return ()
printParagraphs (first:rest) = do
    printParagraph first
    forM_ rest $ \paragraph → do
        replicateM_ 2 (putStrLn "")
        printParagraph paragraph

printParagraph :: [String] → IO ()
printParagraph =
    flip evalStateT (
        PrintTextState
            0
            False
            False
            [setColor Foreground Vivid White]
    )
    .
    mapM_ printWord
  where
    setColor layer intensity color = setSGR [SetColor layer intensity color]
    setSGR' = liftIO ∘ setSGR ∘ (:[])

    printWord :: String → StateT PrintTextState IO ()
    printWord word = do
        let word_length = countVisibleCharsIn word
        number_of_columns ← use current_columns
        let new_number_of_columns = number_of_columns + 1 + word_length
        (case number_of_columns of
            0 → return word_length
            _ | new_number_of_columns > 80 → do
                liftIO $ putStrLn ""
                return word_length
            _ | otherwise → do
                liftIO $ putChar ' '
                return new_number_of_columns
         ) >>= (current_columns .=)
        printChars word

    printChars :: String → StateT PrintTextState IO ()
    printChars [] = return ()
    printChars ('*':xs) = do
        is_bold ← use currently_bold
        if is_bold
            then setSGR' $ SetConsoleIntensity NormalIntensity
            else setSGR' $ SetConsoleIntensity BoldIntensity
        currently_bold %= not
        printChars xs
    printChars ('_':xs) = do
        is_underlined ← use currently_underlined
        if is_underlined
            then setSGR' $ SetUnderlining NoUnderline
            else setSGR' $ SetUnderlining SingleUnderline
        currently_underlined %= not
        printChars xs
    printChars ('[':xs) = do
        let (color_code, '|':rest) = span (/= '|') xs
            ColorSpec intensity color = parseColorSpec color_code
            setThisColor = setColor Foreground intensity color
        liftIO $ setThisColor
        color_restorers %= (setThisColor:)
        printChars rest
    printChars (']':xs) = do
        color_restorers %= tail
        (color_restorer:_) ← use color_restorers
        liftIO $ color_restorer
        printChars xs
    printChars (x:xs) = do
        liftIO $ putChar x
        printChars xs

