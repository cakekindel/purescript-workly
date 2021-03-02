module Test.Logging where

import Prelude

import Effect (Effect)
import Effect.Console (info)
import Effect.Aff (Aff)
import Data.String as S
import Data.Array as A
import Data.Foldable (foldl)

log :: String -> String -> Effect Unit
log prefix msg = info (spaces <> prefix <> " | " <> msg)
  where
    spacesN = 30 - (S.length prefix)
    spaces = A.replicate spacesN " " # foldl (<>) ""
