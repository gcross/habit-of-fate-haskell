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

import HabitOfFate.Logging
import HabitOfFate.Server

browserTestCase ∷ String → (String → WD ()) → TestTree
browserTestCase test_name run =
  testCase test_name $
  withApplication
    (makeApp (secret "test secret") mempty (const $ pure ()))
    (\port →
      runSession defaultConfig
      ∘
      finallyClose
      $
      (setImplicitWait 10000 >> (run $ "http://localhost:" ⊕ show port ⊕ "/"))
    )

main :: IO ()
main = defaultMain ∘ testGroup "All tests" $
  [ browserTestCase "Correct error message when creating with blank username" $ \base_url → do
      openPage $ base_url ⊕ "create"
      findElem (ById "create-account") >>= click
      findElem (ById "error-message") >>= getText >>= liftIO ∘ (@?= no_username_message @?=)
  , browserTestCase "Correct error message when creating with blank username" $ \base_url → do
      openPage $ base_url ⊕ "create"
      findElem (ByName "username") >>= sendKeys "username"
      findElem (ById "create-account") >>= click
      findElem (ById "error-message") >>= getText >>= liftIO ∘ (@?= no_password_message)
  , browserTestCase "Correct error message when creating with no repeated password" $ \base_url → do
      openPage $ base_url ⊕ "create"
      findElem (ByName "username") >>= sendKeys "username"
      findElem (ByName "password") >>= sendKeys "password"
      findElem (ById "create-account") >>= click
      findElem (ById "error-message") >>= getText >>= liftIO ∘ (@?= no_password2_message)
  , browserTestCase "Correct error message when creating with non-matching passwords" $ \base_url → do
      openPage $ base_url ⊕ "create"
      findElem (ByName "username") >>= sendKeys "username"
      findElem (ByName "password") >>= sendKeys "password"
      findElem (ByName "password2") >>= sendKeys "password2"
      findElem (ById "create-account") >>= click
      findElem (ById "error-message") >>= getText >>= liftIO ∘ (@?= password_mismatch_message)
  ]
