{- HLINT ignore "Redundant do" -}

module Main
  ( main
  )
where

import qualified Data.ByteArray as Memory
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Version as Version
import qualified Network.HTTP.Types.QueryLike as Http
import qualified Revolio
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec . Hspec.describe "Revolio" $ do

  Hspec.describe "Type" $ do

    Hspec.describe "PaychexClientId" $ do

      Hspec.it "can be converted into a query value" $ do
        (Http.toQueryValue . Revolio.textToPaychexClientId $ Text.pack "123")
          `Hspec.shouldBe` (Just . Encoding.encodeUtf8 $ Text.pack "123")

    Hspec.describe "SlackSigningSecret" $ do

      Hspec.it "can be used as a byte array" $ do
        (Memory.length . Revolio.textToSlackSigningSecret $ Text.pack "123")
          `Hspec.shouldBe` 3

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
