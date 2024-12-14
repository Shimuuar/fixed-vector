module Main where

import Test.Tasty
import qualified TST.Cereal
import qualified TST.Binary
import qualified TST.CBOR
import qualified TST.Aeson

main :: IO ()
main = defaultMain $ testGroup "fixed-vector"
  [ TST.Cereal.tests
  , TST.Binary.tests
  , TST.CBOR.tests
  , TST.Aeson.tests
  ]
