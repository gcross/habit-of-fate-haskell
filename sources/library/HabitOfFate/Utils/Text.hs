{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Utils.Text where

import Control.Lens (Lens',(%=),(.=),use)
import Control.Lens.TH (makeLenses)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT(),evalStateT)
import Data.Char (isLower, toLower, toUpper)
import Data.List (span)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import System.Console.ANSI
    (Color(..)
    ,ColorIntensity(Dull,Vivid)
    ,ConsoleIntensity(BoldIntensity,NormalIntensity)
    ,ConsoleLayer(Foreground)
    ,SGR(SetConsoleIntensity,SetUnderlining,SetColor)
    ,Underlining(SingleUnderline,NoUnderline)
    ,setSGR
    )

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

substituteAll :: (String → String) → String → String
substituteAll substituteFor = go
  where
    go ('{':rest) =
        let (key,remainder) = break (== '}') rest
        in (substituteFor key) ++ go (tail remainder)
    go (x:rest) = x:go rest
    go [] = []

countVisibleCharsIn :: String → Int
countVisibleCharsIn = go 0
  where
    go accum ('[':xs) = go2 xs
      where
        go2 ('|':ys) = go accum ys
        go2 (_:ys) = go2 ys
        go2 [] = error "No closing ]"
    go accum (x:xs)
      | elem x ['*','_',']'] = go accum xs
      | otherwise = go (accum+1) xs
    go accum [] = accum

data PrintTextState = PrintTextState
    { _current_columns :: Int
    , _currently_bold :: Bool
    , _currently_underlined :: Bool
    , _color_restorers :: [IO ()]
    }
makeLenses ''PrintTextState

printText :: (String → String) → String → IO ()
printText substituteFor =
    flip evalStateT (
        PrintTextState
            0
            False
            False
            [setColor Foreground Vivid White]
    )
    .
    mapM_ printWord
    .
    words
    .
    substituteAll substituteFor
  where
    setColor layer intensity color = setSGR [SetColor layer intensity color]
    setSGR' = liftIO . setSGR . (:[])

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

data Gender = Male | Female | Neuter

makeSubstitutionTable :: [(String,String,Gender)] → Map String String
makeSubstitutionTable = Map.fromList . go mempty
  where
    go :: [(String,String)] → [(String,String,Gender)] → [(String,String)]
    go table [] = table
    go table ((key,value,gender):rest) =
        go ((key,value):article_entries ++ gender_entries ++ table) rest
      where
        article_entries =
            [("a "++key,article_value)
            ,("an "++key,article_value)
            ,("the "++key,"the " ++ value)
            ]
          where
            article
              | elem (toLower . head $ value) ['a','e','i','o','u'] = "an"
              | otherwise = "a"
            article_value = article ++ " " ++ value

        gender_entries = case gender of
            Male → pronouns "he" "him" "man"
            Female → pronouns "she" "her" "woman"
            Neuter → pronouns "it" "it" "thing"
          where
            pronouns subject object category =
                [("he|"++key,subject)
                ,("she|"++key,subject)
                ,("it|"++key,subject)
                ,("him|"++key,object)
                ,("her|"++key,object)
                ,("man|"++key,category)
                ,("woman|"++key,category)
                ,("thing|"++key,category)
                ]
