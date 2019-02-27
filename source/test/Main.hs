{- HLINT ignore "Redundant do" -}

module Main
  ( main
  )
where

import qualified Data.List as List
import qualified Data.Version as Version
import qualified Revolio
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec . Hspec.describe "Revolio" $ do

  Hspec.describe "Version" $ do

    Hspec.describe "version" $ do

      Hspec.it "has four branches" $ do
        Version.versionBranch Revolio.version
          `Hspec.shouldSatisfy` ((== 4) . length)

      Hspec.it "starts branches with one" $ do
        Version.versionBranch Revolio.version
          `Hspec.shouldSatisfy` List.isPrefixOf [1]

      Hspec.it "has no tags" $ do
        versionTags Revolio.version `Hspec.shouldSatisfy` null

versionTags :: Version.Version -> [String]
versionTags (Version.Version _ tags) = tags
