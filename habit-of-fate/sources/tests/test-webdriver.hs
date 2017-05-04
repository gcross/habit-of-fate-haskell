{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main where

import HabitOfFate.Prelude

import Network.Wai.Handler.Warp
import Test.Tasty
import Test.Tasty.HUnit
import Test.WebDriver
import Web.JWT

import HabitOfFate.Client
import HabitOfFate.Logging
import HabitOfFate.Server

main :: IO ()
main = defaultMain ∘ testGroup "All tests" $
  [ testGroup "Create account page" $
      let testCreateAccount test_name run =
            testCase test_name $
            withApplication
              (makeApp (secret "test secret") mempty (const $ pure ()))
              (\port → runSession defaultConfig ∘ finallyClose $ do
                setImplicitWait 10000
                run $ "http://localhost:" ⊕ show port ⊕ "/"
              )
      in
      [ testCreateAccount "Correct error message when creating with blank username" $ \base_url → do
          openPage $ base_url ⊕ "create"
          findElem (ById "create-account") >>= click
          findElem (ById "error-message") >>= getText >>= liftIO ∘ (@?= no_username_message)
      , testCreateAccount "Correct error message when creating with blank username" $ \base_url → do
          openPage $ base_url ⊕ "create"
          findElem (ByName "username") >>= sendKeys "username"
          findElem (ById "create-account") >>= click
          findElem (ById "error-message") >>= getText >>= liftIO ∘ (@?= no_password_message)
      , testCreateAccount "Correct error message when creating with no repeated password" $ \base_url → do
          openPage $ base_url ⊕ "create"
          findElem (ByName "username") >>= sendKeys "username"
          findElem (ByName "password") >>= sendKeys "password"
          findElem (ById "create-account") >>= click
          findElem (ById "error-message") >>= getText >>= liftIO ∘ (@?= no_password2_message)
      , testCreateAccount "Correct error message when creating with non-matching passwords" $ \base_url → do
          openPage $ base_url ⊕ "create"
          findElem (ByName "username") >>= sendKeys "username"
          findElem (ByName "password") >>= sendKeys "password"
          findElem (ByName "password2") >>= sendKeys "password2"
          findElem (ById "create-account") >>= click
          findElem (ById "error-message") >>= getText >>= liftIO ∘ (@?= password_mismatch_message)
      ]
  ]
