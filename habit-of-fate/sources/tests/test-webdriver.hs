{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import HabitOfFate.Prelude

import Test.WebDriver

main :: IO ()
main = do
  runSession (useBrowser firefox defaultConfig) ∘ finallyClose $ do
    openPage "http://localhost:8081/create"
    username_input <- findElem ( ByCSS "input[name='username']" )
    sendKeys "test" username_input
    submit username_input
    openPage "http://localhost:8081/create"
    username_input <- findElem ( ByCSS "input[name='username']" )
    sendKeys "test" username_input
    submit username_input
    openPage "http://localhost:8081/create"
    username_input <- findElem ( ByCSS "input[name='username']" )
    sendKeys "test" username_input
    submit username_input
    openPage "http://localhost:8081/create"
    username_input <- findElem ( ByCSS "input[name='username']" )
    sendKeys "test" username_input
    submit username_input
    openPage "http://localhost:8081/create"
    username_input <- findElem ( ByCSS "input[name='username']" )
    sendKeys "test" username_input
    submit username_input
