{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Testing where

import HabitOfFate.Prelude

import Network.Wai.Handler.Warp

import HabitOfFate.Server

no_files ∷ FileLocator
no_files = const $ pure Nothing

withTestApp ∷ FileLocator → (Int → IO ()) → IO ()
withTestApp locateWebAppFile =
  withApplication
    (makeApp True locateWebAppFile mempty (const $ pure ()))

withTestAppNoFiles ∷ (Int → IO ()) → IO ()
withTestAppNoFiles = withTestApp $ no_files
