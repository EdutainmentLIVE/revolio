{- HLINT ignore "Redundant do" -}

module Main
  ( main
  )
where

import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec . Hspec.describe "Revolio" $ do
  Hspec.it "is" Hspec.pending
