{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2017 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import HabitOfFate.Prelude

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.List
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS

import HabitOfFate.Data.Account
import HabitOfFate.Data.Habit
import HabitOfFate.Logging
import HabitOfFate.Server
import HabitOfFate.TH (textChar8)

testing_certificate = [textChar8|
-----BEGIN CERTIFICATE-----
MIICljCCAX4CCQCWg1jH1c0MhjANBgkqhkiG9w0BAQsFADANMQswCQYDVQQGEwJV
UzAeFw0xNzAyMTIwMjA3MDVaFw0xNzAzMTQwMjA3MDVaMA0xCzAJBgNVBAYTAlVT
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA2IbA2w9ZZFD5oKqkvJAi
epkr2mMdzripBKhI0fUmAhaNd8hZ6c6eepyA1HUN8ITva58l22QJjdKxynoE7QHh
u2fPF6pdBF1RwfCDvUtlAcKqhiJrdxAA8EUa3aAIqNTfAR0kU6GLmj9AFn8Imt/z
7GGEi5m9/DvuQARDJnFmFeZhadyYvQQwtVg1EqCU2UsME9Q9GedurW8Ge0decfiO
HWbDH8TZEfHXXdtCxy1YSqoQ42+hxm4BGSN+L2ZLYfDLwcPTn6d5zHvK0M4L5C6l
hYYMQ7DNHwuW4jL1nU+P/vE6559IkRP9GpH8loqTbr7kre2p4cj2/UmhI8ohXSNv
fwIDAQABMA0GCSqGSIb3DQEBCwUAA4IBAQCAFzdHjv7mBE59vq5evoZvesC+N9aC
JDsuypBoOjBBR1LsKQYvcVm8dMzaUAnE01BJJJ+1w8uUtq2H1stLKEya5Mrv1sHH
O5Ap0EOaszZLapOvickRM0rTfYFHG1uYVChzejVKNkkhbuieqrtMbwphRS8M/S1S
EtFYYRJdTHhEByRVlaNY3Os9LXQarUXl+XTrS5jd9OipmjUR2sSPfxI7qTZJsMEp
s8mNSqXHyukrrhlAfX3p32ymjlA5eNgb888J9WXN072tmUKQi/B8OhUoxJdZPLMW
SPUUbH+1Dx/HvJ5Xc26WxYcgXP221HgzjpJcwPC7bpF6ODcsoejZxDi+
-----END CERTIFICATE-----
|]

testing_key = [textChar8|
-----BEGIN RSA PRIVATE KEY-----
MIIEpAIBAAKCAQEA2IbA2w9ZZFD5oKqkvJAiepkr2mMdzripBKhI0fUmAhaNd8hZ
6c6eepyA1HUN8ITva58l22QJjdKxynoE7QHhu2fPF6pdBF1RwfCDvUtlAcKqhiJr
dxAA8EUa3aAIqNTfAR0kU6GLmj9AFn8Imt/z7GGEi5m9/DvuQARDJnFmFeZhadyY
vQQwtVg1EqCU2UsME9Q9GedurW8Ge0decfiOHWbDH8TZEfHXXdtCxy1YSqoQ42+h
xm4BGSN+L2ZLYfDLwcPTn6d5zHvK0M4L5C6lhYYMQ7DNHwuW4jL1nU+P/vE6559I
kRP9GpH8loqTbr7kre2p4cj2/UmhI8ohXSNvfwIDAQABAoIBAQC07wXBB/Z+6Vtv
cqjuGNN29v+6IhEKaSxzg9w19lCodggJDBZ2Vf4AHz9YSeg4EB2xJPARgGqrZGDE
/WmYU3Y5j+lxsR7BQunK0hyD1bi12+F67NA4Uds268gjYlNaIWeoGp38dIWfgzMe
mVCrenDuGh8UOIiVec8BF461VRUlPjCz+kuRsp29CktVdo2poLZHBToacfm4vHlS
iNFJJRB5Oeu39oUIOMjgQoH/8Lo0qAHbnaUX8uvQazrflNMUVAYM4gpSwMoZS/2b
24QkpOEwp5qk2ADG0x+J4kO9vDyLjhl95b9NzmeeK3hjoW+1n9Wy2dHuXPpOwhwq
FzFEKpuBAoGBAO0Jg+pBhNJAzBos229B9Whr85Q6Gk0HlVaLVmXsN2U6ubetBo7A
lQ8pBLbiG+FF2enSd2WkOnfqWu7sPKxhIAG2nvddTW7g69aeQbMFRNgbBPjtLQvw
r8eFRP+yKCrXbHLEjZ3tEhOhQ+XUnFzO6Qf4oIetSI+TG0OdiVEaiXevAoGBAOnZ
LiURWujhmKZji6c+v8DTmMcXpzEK7HgzscCWGsrXJYOiAKCsSMnPQKF2GV1DvdqS
pT996r5579Rgk5wn1+b3OtDn0XJeZfGUlCnAAAHuxehd7PG/EO4171YyW4jLZd5Q
NeYCnQvNK/PLs4WqvRvNhJuwFaIET2kUQH+nf6kxAoGABh+RzdJlePz9iMuR25zr
lwf64eBiX40LmZG96KIiiDXtKEaK/dMRxrN7kLHkoHwqdfTe6rxEWmaudK+bnaRg
rEqobqF1Z6Dd1sx9y/8d2StRhJCz8jQEhnX141kZ2ol4HrrfIa5HIkSvOVe1tlwN
/wu4MekTD8pwEN4X9wVlebcCgYBdQhd9AObYaN+Pu7M+B3BBXiFXhL9Qd6LGbRc+
Tp5wtkxnqnvFl++PS+/idE65OwLD4Ce2omizfb1/XOSBKgKYQZBbL5f/nFXbef18
iO1319llSc/suN3voeLI5VxLuZHujt5v1Cr9Qd19ZIc7j1PLerKfxCFcc4uXxFPU
lN/8gQKBgQCdgBqjjcNgMToy4Jz0P/afXtVxjiIKZkPG30W7Sdv15Ms7Feraw+hy
+SqqYT4Ovg3EnX+0IEqPJbB+vLgiDM6ndlcIBMQuADmIB0nkP2nfD+Xjl0izGY06
HCkX84dkDHTBizEpVcdZp1gVf/5+hUmFgG/w9EWGrRe2UxhwHJvtkA==
-----END RSA PRIVATE KEY-----
|]

tls_settings ∷ TLSSettings
tls_settings = tlsSettingsMemory testing_certificate testing_key

makeInitialAccounts ∷ IO (Map Username Account)
makeInitialAccounts =
  [ ( ("a", "a")
    ,
      [ ("d6d95381-9a66-453f-a134-64667cb0ef63", Habit "Test 1" (Difficulty Medium) (Importance Medium))
      , ("4e512d1e-99f2-4953-9c9c-9fcbc1e61018", Habit "Test 2" (Difficulty Low) (Importance High))
      , ("7709fdc1-caaf-4c3a-93ee-5ab4137ab653", Habit "Test 3" (Difficulty VeryHigh) (Importance Medium))
      ]
    )
  , ( ("b", "c")
    ,
      [ ("4ea70d5f-b225-4364-a1e5-26693599b221", Habit "Test A" (Difficulty Medium) (Importance Medium))
      ]
    )
  , ( ("c", "b")
    ,
      [ ("b2bfb271-a4ac-4dd8-8974-8055258c858d", Habit "Test α" (Difficulty Low) (Importance Low))
      , ("728b4e0e-a6c3-43ad-9a97-e55d585ab48c", Habit "Test β" (Difficulty Medium) (Importance Medium))
      , ("04fe33fe-048f-4875-9f4d-bfe7ea71013f", Habit "Test c" (Difficulty High) (Importance High))
      ]
    )
  ]
  |> mapM
      (
        \((name, password), habit_list_with_unparsed_ids) → do
          let habit_list = map (first read) habit_list_with_unparsed_ids
              habit_map = habit_list |> mapFromList
              habit_id_seq = unzip habit_list ^. _1 |> fromList
          account ← newAccount password
          pure (Username name, account & habits_ .~ Habits habit_map habit_id_seq)
      )
  |> fmap mapFromList

main = do
  mapM_ logIO
    ["THIS SERVER IS RUNNING IN TEST MODE.  It uses test certificates hard-coded"
    ,"in the binary and it only stores the account data in memory so all account"
    ,"data will be lost when the server exits."
    ]
  accounts_tvar ←
    makeInitialAccounts
    >>=
    \initial_accounts →
      atomically $ traverse newTVar initial_accounts >>= newTVar
  accounts_changed_flag ← newTVarIO False
  makeAppRunningInTestMode accounts_tvar accounts_changed_flag >>=
    runTLS
      (tls_settings { onInsecure = AllowInsecure })
      (setPort 8081 defaultSettings)
