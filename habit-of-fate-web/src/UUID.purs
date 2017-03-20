module UUID where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Data.Char
import Data.Foldable
import Data.String
import Data.Traversable
import Data.Unfoldable
import Partial
import Partial.Unsafe
import Unicode

newtype UUID = UUID String

derive instance eqUUID ∷ Eq UUID
derive instance ordUUID ∷ Ord UUID

instance showUUID ∷ Show UUID where
  show (UUID s) = s

toHexDigit ∷ Partial ⇒ Int → Char
toHexDigit digit
  | digit < 0 = crashWith ("digit " ⊕ show digit ⊕ " is negative")
  | digit < 10 = fromCharCode (toCharCode '0' + digit)
  | digit < 16 = fromCharCode (toCharCode 'a' + (digit-10))
  | otherwise = crashWith ("digit " ⊕ show digit ⊕ " is above 15")

randomHexDigit ∷ ∀ ε. Eff (random ∷ RANDOM | ε) Char
randomHexDigit = unsafePartial toHexDigit <$> randomInt 0 15

randomHexString ∷ ∀ ε. Int → Eff (random ∷ RANDOM | ε) String
randomHexString n =
  fromCharArray
  <$>
  replicateA n randomHexDigit

randomUUID ∷ ∀ ε. Eff (random ∷ RANDOM | ε) UUID
randomUUID = UUID <<< fold <$> sequence
  [ randomHexString 8
  , pure "-"
  , randomHexString 4
  , pure "-"
  , pure "4" -- version 4, i.e randomly generated
  , randomHexString 3
  , pure "-"
  , pure "8" -- I think this means variant 1 (bits 10xx) which is what this is?
  , randomHexString 4
  , pure "-"
  , randomHexString 12
  ]
